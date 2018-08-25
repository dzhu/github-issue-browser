module Search exposing (..)

import Model exposing (Issue)


type Query
    = And (List Query)
    | Or (List Query)
    | Not Query
    | HasAssignee String
    | HasLabel String
    | HasText String


match : Query -> Issue -> Bool
match query issue =
    case query of
        And others ->
            List.all (\q -> match q issue) others

        Or others ->
            List.any (\q -> match q issue) others

        Not other ->
            not (match other issue)

        HasAssignee name ->
            List.member name (List.map .login issue.assignees)

        HasLabel label ->
            List.member label (List.map .name issue.labels)

        HasText text ->
            let
                strings =
                    [ issue.title
                    , issue.body
                    , "#" ++ String.fromInt issue.number
                    ]
                        ++ List.map .name issue.labels
            in
            List.any
                (String.contains (String.toLower text))
                (List.map String.toLower strings)


parse : String -> Query
parse string =
    let
        words =
            String.words string

        wordToQuery word =
            if String.startsWith "-" word then
                Not (wordToQuery (String.dropLeft 1 word))
            else if String.startsWith "label:" word then
                HasLabel (String.dropLeft 6 word)
            else if String.startsWith "l:" word then
                HasLabel (String.dropLeft 2 word)
            else if String.startsWith "assignee:" word then
                HasAssignee (String.dropLeft 9 word)
            else if String.startsWith "a:" word then
                HasAssignee (String.dropLeft 2 word)
            else
                HasText word
    in
    And (List.map wordToQuery words)
