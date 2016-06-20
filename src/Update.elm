module Update exposing (Msg(..), update)

import Http
import Json.Decode as Json exposing ((:=))
import Task exposing (..)
import Time exposing (Time)
import String

import Model exposing (Model, elapsedTimeOf, debounceTime, Repo)


-- Msgs

type Msg
  = Input (Maybe String)
  | SortBy String
  | HideNotice
  | FetchSuccess (Maybe (List Repo))
  | FetchFail Http.Error
  | Tick Time
  | NoOp


-- update

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    -- NoOp
    -- return the current model

    NoOp ->
      ( model, Cmd.none)


    -- Input
    -- set the query value
    -- If the query isn't 'Nothing'
    -- call Tick with the current time.

    Input query ->
      case query of
        Nothing ->
          ( { model
            | query = Nothing
            , notice = Nothing
            }, Cmd.none )

        Just str ->
          let
            model' =
              { model
                | query = Just str
                , notice = Nothing
                , results = Nothing
              }
          in
            if str /= ""
            then
              (model', Time.now |> Task.perform (\_ -> NoOp) Tick)
            else
              (model', Cmd.none)


    -- SortBy
    -- set the filter value
    -- If the query isn't 'Nothing'
    -- call Tick with the current time.

    SortBy filter ->
      case model.query of
        Nothing ->
          ({ model | filter = filter }, Cmd.none)

        Just query ->
          let
            model' =
              { model
                | filter = filter
                , isLoading = True }
          in
            if query /= ""
            then
              (model', Time.now |> Task.perform (\_ -> NoOp) Tick)
          else
            (model', Cmd.none)


    -- HideNotice
    -- remove a notice on click

    HideNotice ->
      if model.notice == Nothing
      then (model, Cmd.none)
      else
        ({ model
           | notice = Nothing
        }, Cmd.none)


    -- FetchSuccess
    -- on success of 'fetch' set the response on the model
    -- if the reponse was an empty list,
    -- define a notice.
    -- set the isLoading state to False

    FetchSuccess results ->
      case results of
        Nothing ->
          ({ model
             | results = Nothing
             , isLoading = False
             , notice = Nothing
            }, Cmd.none)

        Just repos ->
          if repos == []
          then
            ({ model
               | results = Nothing
               , isLoading = False
               , notice = Just "No public repositories found for this user."
              } , Cmd.none)
          else
            ({ model
               | results = Just repos
               , isLoading = False
               , notice = Nothing
              } , Cmd.none)


    -- FetchFail
    -- on Http fail set :
    -- the reponse as 'notice'
    -- the isLoading state to False
    -- result to Nothing

    FetchFail err ->
      case err of
        Http.Timeout ->
          ( { model
              | notice = Just "Timeout"
              , isLoading = False
              , results = Nothing
            }
          , Cmd.none
          )

        Http.NetworkError ->
          ( { model
              | notice = Just "Network Error"
              , isLoading = False
              , results = Nothing
            }
          , Cmd.none
          )

        Http.UnexpectedPayload error ->
          ( { model
              | notice = Just error
              , isLoading = False
              , results = Nothing
            }
          , Cmd.none
          )

        Http.BadResponse code error ->
          ( { model
              | notice = Just error
              , isLoading = False
              , results = Nothing
            }
          , Cmd.none
          )


    -- Tick
    -- using 'elapsedTimeOf' calulate a newElapsedTime
    -- is 'newElapsedTime' the greather than 500 * millisecond ?
    -- if so
    -- set debounceState as Nothing and isLoading to True and perform
    -- a Http request
    -- if not don't make a Http request, but call Tick again

    Tick clockTime ->
      let
        newElapsedTime = elapsedTimeOf model.debounceState clockTime
      in
        if newElapsedTime > debounceTime
        then
          ({ model
             | debounceState = Nothing
             , isLoading = False
           }
           , fetch (String.toLower (Maybe.withDefault "" model.query)) model.filter
          )
        else
          ({ model
           | debounceState = Just { elapsedTime = newElapsedTime, prevClockTime = clockTime }
           , isLoading = True
           }
           , Time.now |> Task.perform (\_ -> NoOp) Tick
          )


-- fetch
-- take a query and a filter and perform a Http request

fetch : String -> String -> Cmd Msg
fetch query filter =
  let url =
    "https://api.github.com/users/" ++ query ++ "/repos?sort=" ++ filter
  in
    Task.perform FetchFail FetchSuccess (Http.get decodeRepos url)


-- decodeRepos
-- return a decoded response we expect to receive

decodeRepos : Json.Decoder (Maybe (List Repo))
decodeRepos =
  let repo =
    Json.object7 Repo
        (Json.maybe ("description" := Json.string))
        ("html_url" := Json.string)
        ("stargazers_count" := Json.int)
        ("watchers_count" := Json.int)
        ("full_name" := Json.string)
        ("owner" := decodeAvatar)
        ("owner" := decodeLogin)
  in
    Json.maybe <| Json.list repo


-- decodeAvatar
-- pluck the avatar from 'owner' in the JSON response

decodeAvatar : Json.Decoder String
decodeAvatar =
  Json.at ["avatar_url"] Json.string


-- decodeLogin
-- pluck the login from 'owner' in the JSON response

decodeLogin : Json.Decoder String
decodeLogin =
  Json.at ["login"] Json.string
