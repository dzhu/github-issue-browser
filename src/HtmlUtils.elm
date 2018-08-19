module HtmlUtils exposing (..)

import Char
import Html exposing (..)
import Html.Events exposing (keyCode, on, onInput, onSubmit)
import Json.Decode as D exposing (Decoder, float, int, list, nullable, string)
import Markdown
import Model exposing (..)


toMarkdown : String -> Html Msg
toMarkdown userInput =
    let
        d =
            Markdown.defaultOptions
    in
    Markdown.toHtmlWith
        { d | sanitize = True }
        []
        userInput


hexDigitValue : Char -> Int
hexDigitValue c =
    let
        d =
            c |> Char.toLower |> Char.toCode
    in
    if 48 <= d && d < 58 then
        d - 48
    else if 97 <= d && d < 103 then
        d - 87
    else
        0


hexStringValue : String -> Int
hexStringValue s =
    let
        hr s =
            case String.uncons s of
                Just ( h, t ) ->
                    16 * hr t + hexDigitValue h

                _ ->
                    0
    in
    hr (String.reverse s)


isDark : String -> Bool
isDark c =
    let
        r =
            String.slice 0 2 c |> hexStringValue

        g =
            String.slice 2 4 c |> hexStringValue

        b =
            String.slice 4 6 c |> hexStringValue

        br =
            0.299 * toFloat r + 0.587 * toFloat g + 0.114 * toFloat b
    in
    br < 130


onKeyPress : (Int -> msg) -> Attribute msg
onKeyPress tagger =
    let
        decoder =
            D.map2
                (\a b ->
                    if a == 0 then
                        b
                    else
                        a
                )
                (D.field "which" int)
                (D.field "keyCode" int)
                |> D.map tagger
    in
    on "keypress" decoder


onKeyUp : (Int -> msg) -> Attribute msg
onKeyUp tagger =
    on "keyup" (D.map tagger keyCode)


targetIndex : Decoder Int
targetIndex =
    D.at [ "target", "selectedIndex" ] int


onChange : (Int -> msg) -> Attribute msg
onChange tagger =
    on "change" (D.map tagger targetIndex)
