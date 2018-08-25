module GitHub exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import Regex


parseLinksHeader : String -> Dict String String
parseLinksHeader str =
    let
        linkRegex =
            Regex.fromString "<(.*)>; rel=\"(.*)\""

        extractLink s =
            case linkRegex of
                Just re ->
                    case List.head (Regex.findAtMost 1 re s) of
                        Just match ->
                            case match.submatches of
                                [ Just a, Just b ] ->
                                    Just ( b, a )

                                _ ->
                                    Nothing

                        _ ->
                            Nothing

                Nothing ->
                    Nothing

        split =
            String.split ", " str
    in
    Dict.fromList (List.filterMap extractLink split)


dictGetCaseInsensitive : String -> Dict String v -> Maybe v
dictGetCaseInsensitive needle =
    Dict.foldl
        (\k v x ->
            case x of
                Just _ ->
                    x

                Nothing ->
                    if String.toLower k == String.toLower needle then
                        Just v
                    else
                        Nothing
        )
        Nothing


ghRequest : String -> Http.Body -> Maybe String -> (Result Http.Error ( Maybe String, a ) -> msg) -> Decoder a -> String -> Cmd msg
ghRequest method body token tag decoder url =
    let
        req =
            Http.request
                { method = method
                , headers = [ Http.header "Authorization" ("token " ++ Maybe.withDefault "" token) ]
                , body = body
                , url = url
                , expect =
                    Http.expectStringResponse
                        (\resp ->
                            let
                                next =
                                    dictGetCaseInsensitive "link" resp.headers
                                        |> Maybe.map parseLinksHeader
                                        |> Maybe.andThen (Dict.get "next")

                                respBody =
                                    Json.Decode.decodeString decoder resp.body
                            in
                            case respBody of
                                Ok x ->
                                    Ok ( next, x )

                                Err x ->
                                    Err <| Json.Decode.errorToString x
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
