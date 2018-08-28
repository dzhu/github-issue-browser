module Model exposing (..)

import Browser.Dom
import Dict exposing (Dict)
import Http
import Time


{-| Messages resulting from responses to HTTP requests.
-}
type ResponseMsg
    = GotIssues (Result Http.Error ( Maybe String, List Issue ))


type Msg
    = NoOp
    | IssueSelected Location Issue
    | SearchChanged String
    | SetPendingToken String
    | SetToken (Maybe String)
    | SetRepo String
    | LogOut
      -- External things.
    | GotStorageValue ( String, Maybe String )
    | DoFocus String
    | FocusDone String (Result Browser.Dom.Error ())
    | InputFocused Bool
    | DoChangeLabels Issue (List String)
    | LabelsChanged Issue (List Label)
    | DoOpenIssueWindow Issue
    | GlobalKeyUp Int
    | Response ResponseMsg
    | GotTimeZone Time.Zone


type alias Label =
    { name : String, color : String }


type alias Issue =
    { number : Int
    , title : String
    , body : String
    , labels : List Label
    , html_url : String
    , assignees : List User
    , user : User
    , isPR : Bool
    , creation_time : Maybe Time.Posix
    }


type alias User =
    { login : String
    , html_url : String
    }


type alias Location =
    { column : Int
    , row : Int
    }


type alias Model =
    { pendingToken : String
    , token : Maybe String
    , search : String
    , repo : String
    , issues : Dict Int Issue
    , loading : Bool
    , inputIsFocused : Bool
    , timeZone : Time.Zone

    -- The currently displayed issue and the location of the highlighted
    , issue : Maybe Issue
    , location : Location
    }
