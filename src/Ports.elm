port module Ports exposing (..)

{-| Ports.
-}

import Model


{-| Put settings into storage.
-}
port storeSettings : Model.Settings -> Cmd msg


{-| Call JS window.open.
-}
port windowOpen : String -> Cmd msg
