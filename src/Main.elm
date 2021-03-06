module Main exposing (..)

import Browser
import Browser.Dom
import Browser.Events
import Dict exposing (Dict)
import GitHub exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, href, id, multiple, placeholder, selected, size, style, tabindex, target, type_, value)
import Html.Events exposing (keyCode, onBlur, onClick, onFocus, onInput, onSubmit, stopPropagationOn)
import Html.Lazy
import HtmlUtils exposing (..)
import Iso8601
import Json.Decode as D exposing (Decoder, float, int, list, nullable, string, succeed)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as E
import Model exposing (..)
import Platform.Sub
import Ports
import Process
import Search exposing (..)
import Set exposing (Set)
import Task
import Time


{-| Configuration: List of priority labels and the columns they should go into.
-}
defaultLabelColumns : List ( String, number )
defaultLabelColumns =
    [ ( "p1", 0 )
    , ( "p2", 1 )
    , ( "p3", 1 )
    ]


{-| All names of priority labels.
-}
labelNames : Settings -> Set String
labelNames settings =
    settings.labelColumns |> List.map Tuple.first |> Set.fromList


textInput : List (Attribute Msg) -> Html Msg
textInput attributes =
    input
        ([ class "input"
         , type_ "text"
         , stopPropagationOn "keydown" (D.succeed ( NoOp, True ))
         , onBlur (InputFocused False)
         , onFocus (InputFocused True)
         ]
            ++ attributes
        )
        []


baseUrl : Model -> String
baseUrl model =
    "https://api.github.com/repos/" ++ model.settings.repo ++ "/"


extraColumnIndex : List ( String, Int ) -> Int
extraColumnIndex labelColumns =
    1 + Maybe.withDefault -1 (List.map Tuple.second labelColumns |> List.maximum)


getIssues : Maybe String -> String -> Cmd Msg
getIssues token url =
    ghGet token (GotIssues >> Response) (list decodeIssue) url


getComments : Maybe String -> Issue -> String -> Cmd Msg
getComments token issue url =
    ghGet token (GotComments issue >> Response) (list decodeComment) url


decodeUser : Decoder User
decodeUser =
    succeed User
        |> required "login" string
        |> required "html_url" string


decodeLabel : Decoder Label
decodeLabel =
    succeed Label
        |> required "name" string
        |> required "color" string


decodeTime : Decoder (Maybe Time.Posix)
decodeTime =
    string
        |> D.map (Iso8601.toTime >> Result.toMaybe)


decodeComment : Decoder Comment
decodeComment =
    succeed Comment
        |> required "id" int
        |> required "created_at" decodeTime
        |> required "user" decodeUser
        |> required "body" string


decodeIssue : Decoder Issue
decodeIssue =
    succeed Issue
        |> required "number" int
        |> required "title" string
        |> required "body" string
        |> required "labels" (list decodeLabel)
        |> required "html_url" string
        |> required "assignees" (list decodeUser)
        |> required "user" decodeUser
        |> optional "pull_request" (succeed True) False
        |> required "created_at" decodeTime
        |> hardcoded Dict.empty


defaultSettings : Settings
defaultSettings =
    { token = Nothing
    , repo = ""
    , labelColumns = defaultLabelColumns
    }


decodeSettings : Decoder Settings
decodeSettings =
    succeed Settings
        |> optional "token" (D.map Just string) defaultSettings.token
        |> optional "repo" string defaultSettings.repo
        |> optional "labelColumns"
            (list (D.map2 Tuple.pair (D.index 0 string) (D.index 1 D.int)))
            defaultSettings.labelColumns


init : D.Value -> ( Model, Cmd Msg )
init value =
    let
        settings =
            case D.decodeValue decodeSettings value of
                Ok s ->
                    s

                Err _ ->
                    defaultSettings
    in
    initWithSettings settings


initWithSettings : Settings -> ( Model, Cmd Msg )
initWithSettings settings =
    let
        hasToken =
            case settings.token of
                Just _ ->
                    True

                _ ->
                    False

        model =
            { pendingToken = ""
            , search = ""
            , issues = Dict.empty
            , location = { column = -1, row = -1 }
            , issue = Nothing
            , loading = hasToken
            , loadingComments = False
            , inputIsFocused = False
            , timeZone = Time.utc
            , labelsChangeToConfirm = Nothing
            , settings = settings
            }
    in
    ( model
    , Cmd.batch
        [ Task.perform GotTimeZone Time.here
        , if hasToken then
            getIssues model.settings.token (baseUrl model ++ "issues?per_page=100")
          else
            Cmd.none
        ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onKeyUp (D.field "keyCode" D.int |> D.map GlobalKeyUp) ]


updateWithResponse : ResponseMsg -> Model -> ( Model, Cmd Msg )
updateWithResponse resp model =
    case resp of
        GotIssues (Err err) ->
            ( model, Cmd.none )

        GotIssues (Ok ( maybeNext, newIssues )) ->
            let
                ( loading, newCmd ) =
                    case maybeNext of
                        Just next ->
                            ( True, getIssues model.settings.token next )

                        _ ->
                            ( False, Cmd.none )
            in
            ( { model
                | issues = Dict.union (newIssues |> List.map (\issue -> ( issue.number, issue )) |> Dict.fromList) model.issues
                , loading = loading
              }
            , newCmd
            )

        GotComments _ (Err err) ->
            ( model, Cmd.none )

        GotComments issue (Ok ( maybeNext, newComments )) ->
            let
                ( loadingComments, newCmd ) =
                    case maybeNext of
                        Just next ->
                            ( True, getComments model.settings.token issue next )

                        _ ->
                            ( False, Cmd.none )

                updateIssue : Int -> Issue -> Issue
                updateIssue =
                    \_ issue2 ->
                        if issue.number == issue2.number then
                            { issue2 | comments = Dict.union (newComments |> List.map (\comment -> ( comment.id, comment )) |> Dict.fromList) issue2.comments }
                        else
                            issue2
            in
            ( { model
                | issues = Dict.map updateIssue model.issues
                , issue = Maybe.map (updateIssue 0) model.issue
                , loadingComments = loadingComments
              }
            , newCmd
            )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        LogOut ->
            let
                ( newModel, _ ) =
                    let
                        s =
                            model.settings
                    in
                    initWithSettings { s | token = Nothing }
            in
            ( newModel, Ports.storeSettings newModel.settings )

        DoOpenIssueWindow issue ->
            ( model, Ports.windowOpen issue.html_url )

        DoFocus id ->
            ( model, Task.attempt (FocusDone id) (Browser.Dom.focus id) )

        ConfirmChangeLabels issue labels ->
            ( { model | labelsChangeToConfirm = Just { issue = issue, labels = labels } }, Cmd.none )

        CancelChangeLabels ->
            ( { model | labelsChangeToConfirm = Nothing }, Cmd.none )

        DoChangeLabels ->
            case model.labelsChangeToConfirm of
                Just { issue, labels } ->
                    let
                        body =
                            E.list E.string labels

                        url =
                            baseUrl model ++ "issues/" ++ String.fromInt issue.number ++ "/labels"

                        procResult result =
                            case result of
                                Err err ->
                                    NoOp

                                Ok ( _, labels2 ) ->
                                    LabelsChanged issue labels2
                    in
                    ( { model | labelsChangeToConfirm = Nothing }
                    , ghPut body model.settings.token procResult (list decodeLabel) url
                    )

                Nothing ->
                    ( model, Cmd.none )

        LabelsChanged issue labels ->
            let
                num =
                    issue.number

                updateIssue =
                    \issue2 ->
                        if issue2.number == num then
                            { issue2 | labels = labels }
                        else
                            issue2

                issues =
                    model.issues
                        |> Dict.map (always updateIssue)
            in
            ( { model | issues = issues, issue = Maybe.map updateIssue model.issue }, Cmd.none )

        FocusDone id result ->
            ( model, Cmd.none )

        InputFocused f ->
            ( { model | inputIsFocused = f, issue = Nothing, location = { column = -1, row = -1 } }, Cmd.none )

        SetPendingToken s ->
            ( { model | pendingToken = s }, Cmd.none )

        SetToken maybeToken ->
            let
                oldSettings =
                    model.settings

                settings =
                    { oldSettings | token = maybeToken }

                newModel =
                    { model | pendingToken = "", settings = settings, loading = True }
            in
            ( newModel
            , Cmd.batch
                [ Ports.storeSettings settings
                , getIssues maybeToken (baseUrl model ++ "issues?per_page=100")
                ]
            )

        SetRepo r ->
            let
                oldSettings =
                    model.settings

                settings =
                    { oldSettings | repo = r }
            in
            ( { model | settings = settings }, Ports.storeSettings settings )

        GlobalKeyUp k ->
            update
                (case model.labelsChangeToConfirm of
                    Just { issue, labels } ->
                        if k == 13 then
                            DoChangeLabels
                        else
                            CancelChangeLabels

                    _ ->
                        if model.inputIsFocused then
                            NoOp
                        else if k == 191 then
                            -- '/' -> focus search
                            DoFocus "search-input"
                        else if k == 79 then
                            -- 'o' -> open issue
                            case model.issue of
                                Just i ->
                                    DoOpenIssueWindow i

                                _ ->
                                    NoOp
                        else if 49 <= k && k < 58 then
                            -- digit -> assign label
                            let
                                ind =
                                    k - 49

                                maybeLabel =
                                    List.drop ind model.settings.labelColumns |> List.head |> Maybe.map Tuple.first
                            in
                            case ( maybeLabel, model.issue ) of
                                ( Just label, Just issue ) ->
                                    let
                                        origLabels =
                                            List.map .name issue.labels |> Set.fromList

                                        labels =
                                            Set.diff origLabels (labelNames model.settings)
                                                |> Set.insert label
                                                |> Set.toList
                                    in
                                    ConfirmChangeLabels issue labels

                                _ ->
                                    NoOp
                        else if k == 82 then
                            -- 'r' -> refresh issue list
                            SetToken model.settings.token
                        else
                            NoOp
                )
                model

        SearchChanged s ->
            ( { model | search = s, issue = Nothing, location = { column = -1, row = -1 } }, Cmd.none )

        IssueSelected sel issue ->
            ( { model | location = sel, issue = Just issue }
            , Process.sleep 750 |> Task.perform (always (DoGetComments issue))
            )

        GotTimeZone zone ->
            ( { model | timeZone = zone }, Cmd.none )

        DoGetComments issue ->
            if Just issue.number == Maybe.map .number model.issue then
                ( { model | loadingComments = True }
                , getComments model.settings.token issue (baseUrl model ++ "issues/" ++ String.fromInt issue.number ++ "/comments")
                )
            else
                ( model, Cmd.none )

        Response resp ->
            updateWithResponse resp model


findColumnIndex : List ( String, Int ) -> Issue -> Int
findColumnIndex labelColumns issue =
    let
        issueLabels =
            List.map .name issue.labels |> Set.fromList

        impl vals =
            case vals of
                [] ->
                    extraColumnIndex labelColumns

                hd :: tl ->
                    if Set.member (Tuple.first hd) issueLabels then
                        Tuple.second hd
                    else
                        impl tl
    in
    impl labelColumns


issueMatch : List ( String, Int ) -> String -> Int -> Issue -> Bool
issueMatch labelColumns str col issue =
    (col == findColumnIndex labelColumns issue)
        && Search.match (Search.parse str) issue


viewIssue : Issue -> Html Msg
viewIssue issue =
    text
        ("#"
            ++ String.fromInt issue.number
            ++ (if issue.isPR then
                    "-PR"
                else
                    ""
               )
            ++ " "
            ++ issue.title
        )


formatTime : Time.Zone -> Time.Posix -> String
formatTime zone time =
    let
        pad n =
            (if n < 10 then
                "0"
             else
                ""
            )
                ++ String.fromInt n

        a f =
            f zone time

        p =
            a >> pad

        m =
            (case a Time.toMonth of
                Time.Jan ->
                    1

                Time.Feb ->
                    2

                Time.Mar ->
                    3

                Time.Apr ->
                    4

                Time.May ->
                    5

                Time.Jun ->
                    6

                Time.Jul ->
                    7

                Time.Aug ->
                    8

                Time.Sep ->
                    9

                Time.Oct ->
                    10

                Time.Nov ->
                    11

                Time.Dec ->
                    12
            )
                |> pad
    in
    String.fromInt (a Time.toYear) ++ "-" ++ m ++ "-" ++ p Time.toDay ++ " " ++ p Time.toHour ++ ":" ++ p Time.toMinute ++ ":" ++ p Time.toSecond


commentCard : User -> Time.Zone -> Time.Posix -> String -> Html Msg
commentCard user zone time markdownBody =
    div [ class "card" ]
        [ header [ class "card-header" ]
            [ p [ class "card-header-title" ]
                [ text user.login
                , span [ class "has-text-weight-normal" ]
                    [ text " at "
                    , text (formatTime zone time)
                    ]
                ]
            ]
        , div [ class "card-content" ]
            [ div [ class "content" ]
                [ if String.isEmpty markdownBody then
                    span [ class "has-text-grey-light is-italic" ] [ text "no body" ]
                  else
                    Html.Lazy.lazy toMarkdown markdownBody
                ]
            ]
        ]


viewIssueFull : Time.Zone -> Issue -> List (Html Msg)
viewIssueFull zone issue =
    [ -- Title.
      span [ class "is-size-3" ]
        [ a [ href issue.html_url, target "_blank" ] [ text issue.title ]
        , text " "
        , span [ class "has-text-grey" ] [ text ("#" ++ String.fromInt issue.number) ]
        ]

    -- Labels and assignee.
    , div [ style "margin" "0 0 0.6em 0" ]
        (List.intersperse (text " ")
            (issue.labels
                |> List.map
                    (\l ->
                        span
                            [ class
                                ("tag has-text-weight-bold "
                                    ++ (if isDark l.color then
                                            "has-text-white"
                                        else
                                            "has-text-black"
                                       )
                                )
                            , style "background-color" ("#" ++ l.color)
                            ]
                            [ text l.name ]
                    )
            )
            ++ [ if List.isEmpty issue.labels then
                    text ""
                 else
                    span [ class "has-text-grey", style "margin" "0 .5em" ] [ text "|" ]
               , text "assigned to: "
               ]
            ++ (if List.isEmpty issue.assignees then
                    [ span [ class "has-text-grey-light is-italic" ] [ text "nobody" ] ]
                else
                    List.intersperse (text ", ")
                        (List.map (\user -> text user.login) issue.assignees)
               )
        )

    -- Issue body.
    , commentCard issue.user
        zone
        (Maybe.withDefault (Time.millisToPosix 0) issue.creation_time)
        issue.body
    ]
        ++ (Dict.values issue.comments
                |> List.map
                    (\comment ->
                        commentCard comment.user
                            zone
                            (Maybe.withDefault (Time.millisToPosix 0) comment.creation_time)
                            comment.body
                    )
           )


issueListColumn : Model -> Int -> Html Msg
issueListColumn model col =
    let
        issues =
            Dict.values model.issues
                |> List.filter (issueMatch model.settings.labelColumns model.search col)
                |> List.reverse

        columnLabels =
            model.settings.labelColumns
                |> List.filterMap
                    (\( l, c ) ->
                        if c == col then
                            Just l
                        else
                            Nothing
                    )
                |> (\labels ->
                        case labels of
                            [] ->
                                [ "other" ]

                            _ ->
                                labels
                   )
                |> List.map text
                |> List.intersperse (text " / ")
    in
    div [ class "column" ]
        [ div [ class "has-text-centered has-text-weight-bold" ]
            (columnLabels ++ [ text <| " (" ++ String.fromInt (List.length issues) ++ ")" ])
        , div
            [ classList
                [ ( "select", True )
                , ( "is-multiple", True )
                , ( "is-flex", True )
                , ( "is-loading", model.loading )
                ]
            , style "width" "100%"
            ]
            [ select
                [ -- Force it to fill the column and match heights with all
                  -- other selects.
                  style "height" "initial"
                , style "padding" "0"
                , style "width" "100%"
                , style "background-color" "initial"
                , disabled (List.isEmpty issues)
                , onChange
                    (\ind ->
                        case List.head (List.drop ind issues) of
                            Just issue ->
                                IssueSelected { column = col, row = ind } issue

                            _ ->
                                NoOp
                    )
                , size 10
                ]
                (issues
                    |> List.indexedMap
                        (\i issue ->
                            option
                                [ selected (model.location == { column = col, row = i })
                                ]
                                [ viewIssue issue ]
                        )
                )
            ]
        ]


keybindHelp : Settings -> List ( String, String )
keybindHelp settings =
    [ ( "/", "focus search box" )
    , ( "o", "open current issue in new window" )
    , ( "r", "refresh issue list" )
    ]
        ++ List.indexedMap (\i l -> ( String.fromInt (i + 1), "set issue priority to \"" ++ Tuple.first l ++ "\"" )) settings.labelColumns


searchHelp : Html Msg
searchHelp =
    let
        tt t =
            code [] [ text t ]
    in
    div []
        [ p [] [ text "search text is split by spaces (no quoting yet); words are searched for in title, body, and label names" ]
        , ul []
            [ li [] [ tt "a:", text " or ", tt "assignee:", text ": search by assignee login name" ]
            , li [] [ tt "l:", text " or ", tt "label:", text ": search by label" ]
            , li [] [ tt "-", text ": negate word" ]
            ]
        ]


help : Settings -> Html Msg
help settings =
    div [ class "content" ]
        [ div [ class "is-size-4" ] [ text "Keybindings" ]
        , ul []
            (List.map
                (\info ->
                    li []
                        [ code [] [ text (Tuple.first info) ]
                        , text (": " ++ Tuple.second info)
                        ]
                )
                (keybindHelp settings)
            )
        , div [ class "is-size-4" ] [ text "Search" ]
        , searchHelp
        ]


viewUnauthorized : Model -> Html Msg
viewUnauthorized model =
    div []
        [ div [ class "content" ]
            [ a [ href "https://github.com/settings/tokens/new", target "_blank" ]
                [ text "get token here (check the ", b [] [ text "repo" ], text " checkbox)" ]
            , br [] []
            , text "token is stored locally; all communication is only done directly with GitHub"
            ]
        , form [ onSubmit (SetToken (Just model.pendingToken)) ]
            [ input [ type_ "submit", class "is-hidden" ] []
            , div [ class "field" ]
                [ div [ class "control" ]
                    [ textInput [ id "search-input", type_ "password", placeholder "GitHub API token", value model.pendingToken, onInput SetPendingToken ] ]
                ]
            , div [ class "field" ]
                [ div [ class "control" ]
                    [ textInput [ placeholder "GitHub repo (\"owner/name\")", value model.settings.repo, onInput SetRepo ] ]
                ]
            ]
        , help model.settings
        ]


view : Model -> Browser.Document Msg
view model =
    let
        title =
            "GitHub issue browser"
                ++ (case model.issue of
                        Just issue ->
                            ": #" ++ String.fromInt issue.number ++ " " ++ issue.title

                        Nothing ->
                            ""
                   )

        confirmModal =
            case model.labelsChangeToConfirm of
                Just _ ->
                    div
                        [ style "position" "absolute"
                        , style "left" "0"
                        , style "right" "0"
                        , style "top" "0"
                        , style "bottom" "0"
                        , style "background-color" "#00000050"
                        , style "z-index" "1"
                        ]
                        [ div
                            [ class "has-text-danger is-size-3"
                            , style "position" "relative"
                            , style "float" "left"
                            , style "top" "50%"
                            , style "left" "50%"
                            , style "transform" "translate(-50%,-50%)"
                            ]
                            [ text "press enter to confirm" ]
                        ]

                Nothing ->
                    text ""

        body =
            case model.settings.token of
                Nothing ->
                    [ viewUnauthorized model ]

                _ ->
                    [ confirmModal
                    , div []
                        [ div [ class "field is-grouped" ]
                            [ div [ class "control" ]
                                [ button [ class "button", onClick LogOut ] [ text "log out" ]
                                ]
                            , div
                                [ classList
                                    [ ( "control", True )
                                    , ( "is-expanded", True )
                                    , ( "is-loading", model.loading )
                                    ]
                                ]
                                [ textInput
                                    [ id "search-input"
                                    , placeholder "Search..."
                                    , type_ "text"
                                    , onInput SearchChanged
                                    ]
                                ]
                            ]
                        , div
                            [ class "columns is-gapless", style "margin-bottom" "0" ]
                            (List.map (issueListColumn model) (List.range 0 (extraColumnIndex model.settings.labelColumns)))
                        , div [ class "columns is-centered", style "margin" "0" ]
                            [ div [ class "column is-10" ]
                                (Maybe.withDefault [ help model.settings ] (Maybe.map (viewIssueFull model.timeZone) model.issue))
                            ]
                        , if model.loadingComments then
                            div [ class "has-text-centered" ]
                                [ div [ class "icon loader is-large" ] [] ]
                          else
                            text ""
                        ]
                    ]
    in
    { title = title, body = body }


main : Program D.Value Model Msg
main =
    Browser.document { init = init, view = view, update = update, subscriptions = subscriptions }
