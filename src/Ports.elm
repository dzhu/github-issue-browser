port module Ports exposing (..)

{-| Ports.
-}


{-| Local storage.
-}
port set : ( String, Maybe String ) -> Cmd msg


port del : String -> Cmd msg


port get : String -> Cmd msg


port recv : (( String, Maybe String ) -> msg) -> Sub msg


{-| Call JS window.open.
-}
port windowOpen : String -> Cmd msg
