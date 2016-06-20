module Model exposing (Model, Repo, model, elapsedTimeOf, debounceTime)

import Time exposing (Time, millisecond)


-- Model
-- define a structure for the Model

type alias Model =
  { debounceState : DebounceState
  , filter : String
  , isLoading : Bool
  , notice : Maybe String
  , results : Maybe (List Repo)
  , query : Maybe String
  }


-- Repo
-- define a structure for Repo


type alias Repo =
  { description : Maybe String
  , html_url : String
  , stargazers_count : Int
  , watchers_count : Int
  , full_name : String
  , avatar_url : String
  , login : String
  }


-- DebounceState
-- define a structure for DebounceState

type alias DebounceState = Maybe
  { elapsedTime : Time
  , prevClockTime : Time
  }


-- model
-- the state of the app

model : Model
model =
  { debounceState = Nothing
  , filter = "full_name"
  , isLoading = False
  , results = Nothing
  , notice = Nothing
  , query = Nothing
  }


-- debounceTime
-- define a duration to debouce a request

debounceTime : Time
debounceTime = 800 * millisecond


-- elapsedTimeOf
-- calculate the elapsedTime + (clockTime - prevClockTime)

elapsedTimeOf : DebounceState -> Time -> Time
elapsedTimeOf debounceState clockTime =
  case debounceState of
    Nothing -> 0
    Just { elapsedTime, prevClockTime } -> elapsedTime + (clockTime - prevClockTime)
