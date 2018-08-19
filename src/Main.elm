module Main exposing (..)

import Dom
import GitHub exposing (..)
import Html exposing (..)
import Html.Attributes exposing (class, classList, disabled, href, id, multiple, placeholder, selected, size, style, tabindex, target, type_, value)
import Html.Events exposing (keyCode, onBlur, onClick, onFocus, onInput, onSubmit, onWithOptions)
import HtmlUtils exposing (..)
import Json.Decode as D exposing (Decoder, float, int, list, nullable, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, optional, required)
import Json.Encode as E
import Keyboard
import Model exposing (..)
import Platform.Sub
import Ports
import Set exposing (Set)
import Task


{-| Configuration: List of priority labels and the columns they should go into.
-}
priorityLabelColumns : List ( String, number )
priorityLabelColumns =
    [ ( "p1", 0 )
    , ( "p2", 1 )
    , ( "p3", 1 )
    ]


{-| All names of priority labels.
-}
priorityLabels : Set String
priorityLabels =
    priorityLabelColumns |> List.map Tuple.first |> Set.fromList


textInput : List (Attribute Msg) -> Html Msg
textInput attributes =
    input
        ([ class "input"
         , type_ "text"
         , onWithOptions "keydown" { stopPropagation = True, preventDefault = False } (D.succeed NoOp)
         ]
            ++ attributes
        )
        []


baseUrl : Model -> String
baseUrl model =
    "https://api.github.com/repos/" ++ model.repo ++ "/"


extraColumnIndex : Int
extraColumnIndex =
    1 + Maybe.withDefault -1 (List.map Tuple.second priorityLabelColumns |> List.maximum)


getIssues : Maybe String -> String -> Cmd Msg
getIssues token url =
    ghGet token (GotIssues >> Response) (list decodeIssue) url


decodeUser : Decoder User
decodeUser =
    decode User
        |> required "login" string
        |> required "html_url" string


decodeLabel : Decoder Label
decodeLabel =
    decode Label
        |> required "name" string
        |> required "color" string


decodeIssue : Decoder Issue
decodeIssue =
    decode Issue
        |> required "number" int
        |> required "title" string
        |> required "body" string
        |> required "labels" (list decodeLabel)
        |> required "html_url" string
        |> required "assignees" (list decodeUser)
        |> required "user" decodeUser
        |> optional "pull_request" (decode True) False


init : ( Model, Cmd Msg )
init =
    let
        model =
            { pendingToken = ""
            , token = Nothing
            , search = ""
            , repo = ""
            , issues = []
            , location = { column = -1, row = -1 }
            , issue = Nothing
            }
    in
    ( model
    , Cmd.batch [ Ports.get "token", Ports.get "repo" ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs GlobalKeyUp
        , Ports.recv GotStorageValue
        ]


updateWithResponse : ResponseMsg -> Model -> ( Model, Cmd Msg )
updateWithResponse resp model =
    case resp of
        GotIssues (Err err) ->
            let
                _ =
                    Debug.log "Error in getting issues" err
            in
            ( model, Cmd.none )

        GotIssues (Ok ( maybeNext, newIssues )) ->
            let
                newCmd =
                    case maybeNext of
                        Just next ->
                            getIssues model.token next

                        _ ->
                            Cmd.none
            in
            ( { model | issues = model.issues ++ newIssues }, newCmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotStorageValue ( key, value ) ->
            let
                msg =
                    case key of
                        "token" ->
                            SetToken value

                        "repo" ->
                            SetRepo (Maybe.withDefault "" value)

                        _ ->
                            NoOp
            in
            update msg model

        LogOut ->
            let
                ( newModel, _ ) =
                    init
            in
            ( { newModel | repo = model.repo }, Ports.del "token" )

        DoOpenIssueWindow issue ->
            ( model, Ports.windowOpen issue.html_url )

        DoFocus id ->
            ( model, Task.attempt (FocusDone id) (Dom.focus id) )

        DoChangeLabels issue labels ->
            let
                body =
                    E.list (List.map E.string labels)

                url =
                    baseUrl model ++ "issues/" ++ toString issue.number ++ "/labels"

                procResult result =
                    case result of
                        Err err ->
                            NoOp

                        Ok ( _, labels ) ->
                            LabelsChanged issue labels
            in
            ( model
            , ghPut body model.token procResult (list decodeLabel) url
            )

        LabelsChanged issue labels ->
            let
                num =
                    issue.number

                updateIssue =
                    \issue ->
                        if issue.number == num then
                            { issue | labels = labels }
                        else
                            issue

                issues =
                    model.issues
                        |> List.map updateIssue
            in
            ( { model | issues = issues, issue = Maybe.map updateIssue model.issue }, Cmd.none )

        FocusDone id result ->
            let
                _ =
                    case result of
                        Err err ->
                            let
                                _ =
                                    Debug.log "Error in focusing" ( id, err )
                            in
                            ()

                        _ ->
                            ()
            in
            ( model, Cmd.none )

        SetPendingToken s ->
            ( { model | pendingToken = s }, Cmd.none )

        SetToken maybeToken ->
            ( { model | pendingToken = "", token = maybeToken }
            , Cmd.batch
                [ case maybeToken of
                    Just _ ->
                        Cmd.batch
                            [ Ports.set ( "token", maybeToken )
                            , getIssues maybeToken (baseUrl model ++ "issues?per_page=100")
                            ]

                    Nothing ->
                        Ports.del "token"
                ]
            )

        SetRepo r ->
            ( { model | repo = r }, Ports.set ( "repo", Just r ) )

        GlobalKeyUp k ->
            update
                (if k == 191 then
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

                        label =
                            List.drop ind priorityLabelColumns |> List.head |> Maybe.map Tuple.first
                    in
                    case ( label, model.issue ) of
                        ( Just label, Just issue ) ->
                            let
                                origLabels =
                                    List.map .name issue.labels |> Set.fromList

                                labels =
                                    origLabels
                                        |> flip Set.diff priorityLabels
                                        |> Set.insert label
                                        |> Set.toList
                            in
                            DoChangeLabels issue labels

                        _ ->
                            NoOp
                 else
                    NoOp
                )
                model

        SearchChanged s ->
            ( { model | search = s, issue = Nothing, location = { column = -1, row = -1 } }, Cmd.none )

        IssueSelected sel issue ->
            ( { model | location = sel, issue = Just issue }, Cmd.none )

        Response resp ->
            updateWithResponse resp model


findColumnIndex : Issue -> Int
findColumnIndex issue =
    let
        issueLabels =
            List.map .name issue.labels |> Set.fromList

        impl vals =
            case vals of
                [] ->
                    extraColumnIndex

                hd :: tl ->
                    if Set.member (Tuple.first hd) issueLabels then
                        Tuple.second hd
                    else
                        impl tl
    in
    impl priorityLabelColumns


issueMatch : String -> Int -> Issue -> Bool
issueMatch str col issue =
    let
        targetCol =
            findColumnIndex issue

        strings =
            [ issue.title, issue.body, "#" ++ toString issue.number ] ++ List.map .name issue.labels
    in
    (col == targetCol)
        && List.any (String.contains (String.toLower str)) (List.map String.toLower strings)


viewIssue : Issue -> Html Msg
viewIssue issue =
    text
        ("#"
            ++ toString issue.number
            ++ (if issue.isPR then
                    "-pr"
                else
                    ""
               )
            ++ " "
            ++ issue.title
        )


viewIssueFull : Issue -> Html Msg
viewIssueFull issue =
    div [ class "columns is-centered", style [ ( "margin", "0" ) ] ]
        [ div [ class "column is-10" ]
            [ -- Title.
              span [ class "is-size-3" ]
                [ a [ href issue.html_url, target "_blank" ] [ text issue.title ]
                , text " "
                , span [ class "has-text-grey" ] [ text ("#" ++ toString issue.number) ]
                ]

            -- User info (opener and assignees).
            , div []
                ([ text "by: " ]
                    ++ [ text issue.user.login ]
                    ++ [ span [ class "has-text-grey", style [ ( "margin", "0 .5em" ) ] ] [ text "|" ] ]
                    ++ [ text "assigned to: " ]
                    ++ (if List.isEmpty issue.assignees then
                            [ span [ class "has-text-grey-light is-italic" ] [ text "nobody" ] ]
                        else
                            List.intersperse (text ", ")
                                (List.map (\user -> text user.login) issue.assignees)
                       )
                )

            -- Label list.
            , div [ style [ ( "margin", ".6em 0" ) ] ]
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
                                    , style
                                        [ ( "background-color", "#" ++ l.color )
                                        ]
                                    ]
                                    [ text l.name ]
                            )
                    )
                )

            -- Issue body.
            , div [ class "content" ]
                [ if String.isEmpty issue.body then
                    span [ class "has-text-grey-light is-italic" ] [ text "no body" ]
                  else
                    toMarkdown issue.body
                ]
            ]
        ]


issueListColumn : Model -> Int -> Html Msg
issueListColumn model col =
    let
        issues =
            List.filter (issueMatch model.search col) model.issues

        columnLabels =
            priorityLabelColumns
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
    div [ class "column is-one-third" ]
        [ div [ class "has-text-centered has-text-weight-bold" ]
            (columnLabels ++ [ text <| " (" ++ toString (List.length issues) ++ ")" ])
        , div [ class "select is-multiple is-flex", style [ ( "width", "100%" ) ] ]
            [ select
                [ -- Force it to fill the column and match heights with all
                  -- other selects.
                  style
                    [ ( "height", "initial" )
                    , ( "padding", "0" )
                    , ( "width", "100%" )
                    , ( "background-color", "initial" )
                    ]
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


keybindInfo : List ( String, String )
keybindInfo =
    [ ( "/", "focus search box (search is very basic: case-insensitive substring search within labels, titles and bodies)" )
    , ( "o", "open current issue in new window" )
    ]
        ++ List.indexedMap (\i l -> ( toString (i + 1), "set issue priority to \"" ++ Tuple.first l ++ "\"" )) priorityLabelColumns


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
                    [ textInput [ placeholder "GitHub repo (\"owner/name\")", value model.repo, onInput SetRepo ] ]
                ]
            ]
        , div [ class "content" ]
            [ span [ class "is-size-4" ] [ text "Keybindings" ]
            , ul []
                (List.map
                    (\info ->
                        li []
                            [ code [] [ text (Tuple.first info) ]
                            , text (": " ++ Tuple.second info)
                            ]
                    )
                    keybindInfo
                )
            ]
        ]


view : Model -> Html Msg
view model =
    case model.token of
        Nothing ->
            viewUnauthorized model

        _ ->
            div []
                [ form []
                    [ div [ class "field is-grouped" ]
                        [ div [ class "control" ]
                            [ button [ class "button", onClick LogOut ] [ text "log out" ]
                            ]
                        , div [ class "control is-expanded" ]
                            [ textInput
                                [ id "search-input"
                                , placeholder "Search..."
                                , type_ "text"
                                , onInput SearchChanged
                                ]
                            ]
                        ]
                    ]
                , div
                    [ class "columns is-gapless" ]
                    (List.map (issueListColumn model) (List.range 0 extraColumnIndex))
                , Maybe.withDefault (span [] []) (Maybe.map viewIssueFull model.issue)
                ]


main : Program Never Model Msg
main =
    program { init = init, view = view, update = update, subscriptions = subscriptions }
