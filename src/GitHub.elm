module GitHub exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import Regex


parseLinksHeader : String -> Dict String String
parseLinksHeader str =
    let
        extractLink s =
            case List.head (Regex.find (Regex.AtMost 1) (Regex.regex "<(.*)>; rel=\"(.*)\"") s) of
                Just match ->
                    case match.submatches of
                        [ Just a, Just b ] ->
                            Just ( b, a )

                        _ ->
                            Nothing

                Nothing ->
                    Nothing

        split =
            String.split ", " str
    in
    Dict.fromList (List.filterMap extractLink split)


ghRequest : String -> Http.Body -> Maybe String -> (Result Http.Error ( Maybe String, a ) -> msg) -> Decoder a -> String -> Cmd msg
ghRequest method body token tag decoder url =
    let
        req =
            Http.request
                { method = method
                , headers =
                    [ Http.header "User-Agent" "issues-test by dzhu"
                    , Http.header "Authorization" ("token " ++ Maybe.withDefault "" token)
                    ]
                , body = body
                , url = url
                , expect =
                    Http.expectStringResponse
                        (\resp ->
                            let
                                next =
                                    Dict.get "Link" resp.headers
                                        |> Maybe.map parseLinksHeader
                                        |> Maybe.andThen (Dict.get "next")

                                body =
                                    Json.Decode.decodeString decoder resp.body
                            in
                            case body of
                                Ok x ->
                                    Ok ( next, x )

                                Err x ->
                                    Err x
                        )
                , timeout = Nothing
                , withCredentials = False
                }
    in
    Http.send tag req


ghGet : Maybe String -> (Result Http.Error ( Maybe String, a ) -> msg) -> Decoder a -> String -> Cmd msg
ghGet =
    ghRequest "GET" Http.emptyBody


ghPost : Json.Encode.Value -> Maybe String -> (Result Http.Error ( Maybe String, a ) -> msg) -> Decoder a -> String -> Cmd msg
ghPost body =
    ghRequest "POST" (Http.jsonBody body)


ghPut : Json.Encode.Value -> Maybe String -> (Result Http.Error ( Maybe String, a ) -> msg) -> Decoder a -> String -> Cmd msg
ghPut body =
    ghRequest "PUT" (Http.jsonBody body)


ghDelete : Maybe String -> (Result Http.Error ( Maybe String, a ) -> msg) -> Decoder a -> String -> Cmd msg
ghDelete =
    ghRequest "DELETE" Http.emptyBody
