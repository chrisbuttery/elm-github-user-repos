module Main where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Json exposing ((:=))
import Effects exposing (Never, Effects)
import Task exposing (..)
import StartApp
import Signal
import Time exposing (Time, millisecond)
import String

import Styles

import Debug exposing (..)


-- MODEL


{--
  Alias Repo:
  Has the shape of a record
  Has a full_name prop that is of string type
  Has a description prop that 'Maybe' is a String
  Has a html_url prop that is of string type
  Has a stargazers_count that is of type Int
  Has a watchers_count that is of type Int
--}

type alias Repo =
  { full_name: String
  , description: Maybe String
  , html_url: String
  , stargazers_count: Int
  , watchers_count: Int
  }


{--
  Alias Model:
  Has the shape of a record
  Has a query prop that is of sting type
  Has a results prop that 'Maybe' is a List of Repos or Nothing
  Has a debounceState that is of type 'DebounceState'
  Has a message that 'Maybe' is a string or Nothing
--}

type alias Model = {
  query : String
  , results : Maybe (List Repo)
  , debounceState : DebounceState
  , message : Maybe String
}


{--
  DebounceState type:
  Will 'Maybe' have the shape of a record or Nothing
  Has a query prop that is of sting type
  Has a prevClockTime prop that is of type Time
  Has a elapsedTime prop that is of type Time
--}

type alias DebounceState = Maybe {
  prevClockTime : Time
  , elapsedTime: Time
}


{--
  Model (record)
  Sets a query prop to an empty string
  Sets results as Nothing, as this value 'maybe' a string or nothing
  Sets debounceState to Nothing, as this type is a 'Maybe'
--}

model : (Model, Effects Action)
model =
  ({
  query = ""
  , results = Nothing
  , debounceState = Nothing
  , message = Nothing
  }
  , Effects.none )


{--
  debounceTime = half a second
--}

debounceTime : Time
debounceTime = 500 * millisecond


{--
  elapsedTimeOf
  takes debounceState and a clockTime
  if debounceState Nothing, return 0
  otherwise get 'elapsedTime' and 'prevClockTime'
  and return 'elapsedTime' plus 'clockTime' minus 'prevClockTime'
--}

elapsedTimeOf : DebounceState -> Time -> Time
elapsedTimeOf debounceState clockTime =
  case debounceState of
    Nothing -> 0
    Just { elapsedTime, prevClockTime } -> elapsedTime + (clockTime - prevClockTime)


-- UPDATE

{--
  Action = Union Type
  It can either be:
  Input: which is a String
  UpdateResults: which 'might' be a String
--}

type Action = Input String
  | UpdateResults (Maybe (List Repo))
  | Tick Time

{--
  Update:
  Requires an address and model
  If the address was:
  Input - Update the query with the accompaning string
  UpdateResults - update results with a possible value
--}

update : Action -> Model -> (Model, Effects Action)
update address model =
  case address of

    {--
      on 'Input'
      - check the model's debounceState record
      - if it's Nothing, set the query on the model an trigger 'Tick'
      - otherwise set the query and set 'debounceState' to Nothing and
        don't trigger anything
    --}

    Input query ->
      case model.debounceState of
        Nothing ->
          ({ model | query = query }, Effects.tick Tick)

        Just oldDebounceState ->
          ({ model | query = query, debounceState = Nothing }, Effects.none)

    {--
      on 'UpdateResults'
      if there's no query, set message to Nothing
      otherwise if no results exists, set 'message' to Nothing or a String
      set the results  on the model, don't trigger anything
    --}

    UpdateResults results ->
      let
        msg =
          if model.query == "" then Nothing else
            if List.length (Maybe.withDefault [] results) == 0
            then Just ("Github user '" ++ (String.toLower model.query) ++ "' not found :(")
            else Nothing
      in
        ({ model | results = results, message = msg }, Effects.none)

    {--
      on 'Tick'
      first calculate the new newElapsedTime since debounceState was Nothing
      if the 'newElapsedTime' in greather than 'debounceTime' (500ms)
      - set debounceState to Nothing on the model and perform HTTP request with the query
      otherwise set the new debounceState on the model and trigger another Tick
    --}

    Tick clockTime ->
      let
        newElapsedTime = elapsedTimeOf model.debounceState clockTime
      in
        if newElapsedTime > debounceTime then
          ({model | debounceState = Nothing}, search (String.toLower model.query))
        else
          ({ model | debounceState = Just { elapsedTime = newElapsedTime, prevClockTime = clockTime } }
            , Effects.tick Tick)

{--
  search:
  Look up the github Url
  - decode response with 'repos'
  - call the Action 'UpdateResults' passing it the result
--}

search : String -> Effects Action
search query =
  Http.get repos ("https://api.github.com/users/" ++ query ++ "/repos")
    |> Task.toMaybe
    |> Task.map UpdateResults
    |> Effects.task


{--
  repos:
  the result of the HTTP request = an array or objects
  Get specific properties from each object
--}

repos : Json.Decoder (List Repo)
repos =
  let repo =
    Json.object5 Repo
        ("full_name" := Json.string)
        (Json.maybe ("description" := Json.string))
        ("html_url" := Json.string)
        ("stargazers_count" := Json.int)
        ("watchers_count" := Json.int)
  in
      Json.list repo


-- VIEW

{--
  header:
  Returns HTML
--}

header : List Html
header =
  [ text "Snoophub" ]

{--
  result:
  Takes a model and returns HTML
--}

result : Repo -> Html
result item =
  li [ Styles.result ] [
    div [ Styles.meta ] [
      div [] [ text ("Stars: " ++ (toString item.stargazers_count)) ]
      , div [] [ text ("Watchers: " ++ (toString item.watchers_count)) ]
    ]
    , div [ Styles.copy ] [
      a [ href item.html_url , title item.full_name, Styles.name ] [ text item.full_name ]
      , div [] [ text (Maybe.withDefault "" item.description)]
    ]
  ]

{--
  view:
  Takes a Signal Address action and a model.
  On input we explicity tell it to send the targetValue to the address action 'Input'
  'targetValue' becomes the 'query' param in our update function
  If the 'message' is Nothing, hide the message element, otherwise style it.
  Map through results and send off to 'result'
--}

view : Signal.Address Action -> Model -> Html
view address model =
  div [ Styles.container ] [
    div [ Styles.header ] (header)
    , input [
      placeholder "Enter a Github username"
      , value model.query
      , on "input" targetValue (Signal.message (Signal.forwardTo address Input))
      , Styles.input
    ] []
    , div [ if model.message == Nothing then Styles.hidden else Styles.message ]
      [ text (Maybe.withDefault "" model.message) ]
    , ul [ Styles.results ] (List.map result (Maybe.withDefault [] model.results))
  ]


-- START

app =
  StartApp.start {
    init = model
    , view = view
    , update = update
    , inputs = []
  }

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks