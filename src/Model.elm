module Model exposing (..)

import Browser.Dom
import Dict exposing (Dict)
import Http
import Time


{-| Messages resulting from responses to HTTP requests. The tuple value contains
a `next` link, if any, and the request payload.
-}
type ResponseMsg
    = GotIssues (Result Http.Error ( Maybe String, List Issue ))
    | GotComments Issue (Result Http.Error ( Maybe String, List Comment ))


type Msg
    = NoOp
    | IssueSelected Location Issue
    | SearchChanged String
    | SetPendingToken String
    | SetToken (Maybe String)
    | SetRepo String
    | LogOut
      -- External things.
    | DoFocus String
    | FocusDone String (Result Browser.Dom.Error ())
    | InputFocused Bool
    | ConfirmChangeLabels Issue (List String)
    | DoChangeLabels
    | CancelChangeLabels
    | LabelsChanged Issue (List Label)
    | DoGetComments Issue
    | DoOpenIssueWindow Issue
    | GlobalKeyUp Int
    | Response ResponseMsg
    | GotTimeZone Time.Zone


type alias Label =
    { name : String
    , color : String
    }


type alias User =
    { login : String
    , html_url : String
    }


type alias Comment =
    { id : Int
    , creation_time : Maybe Time.Posix
    , user : User
    , body : String
    }


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
    , comments : Dict Int Comment
    }


type alias Location =
    { column : Int
    , row : Int
    }


type alias Settings =
    { token : Maybe String
    , repo : String
    , labelColumns : List ( String, Int )
    }


type alias Model =
    { pendingToken : String
    , search : String
    , issues : Dict Int Issue
    , loading : Bool
    , loadingComments : Bool
    , inputIsFocused : Bool
    , timeZone : Time.Zone
    , labelsChangeToConfirm : Maybe { issue : Issue, labels : List String }
    , settings : Settings

    -- The currently displayed issue and the location of the highlighted
    , issue : Maybe Issue
    , location : Location
    }
