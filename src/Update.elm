module Update exposing (Msg(..), update)

import Http
import Json.Decode as Json exposing (field)
import Task exposing (..)
import Time exposing (Time)
import String
import Model exposing (Model, elapsedTimeOf, debounceTime, Repo)
import Debug


-- Msgs


type Msg
    = Input (Maybe String)
    | SortBy String
    | HideNotice
    | FetchSuccess (Result Http.Error (Maybe (List Repo)))
    | Tick Time
    | NoOp



-- update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- NoOp
        -- return the current model
        NoOp ->
            ( model, Cmd.none )

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
                      }
                    , Cmd.none
                    )

                Just str ->
                    let
                        model_ =
                            { model
                                | query = Just str
                                , notice = Nothing
                                , results = Nothing
                            }
                    in
                        if str /= "" then
                            ( model_, Task.perform Tick Time.now )
                        else
                            ( model_, Cmd.none )

        -- SortBy
        -- set the filter value
        -- If the query isn't 'Nothing'
        -- call Tick with the current time.
        SortBy filter ->
            case model.query of
                Nothing ->
                    ( { model | filter = filter }, Cmd.none )

                Just query ->
                    let
                        model_ =
                            { model
                                | filter = filter
                                , isLoading = True
                            }
                    in
                        if query /= "" then
                            ( model_, Task.perform Tick Time.now )
                        else
                            ( model_, Cmd.none )

        -- HideNotice
        -- remove a notice on click
        HideNotice ->
            if model.notice == Nothing then
                ( model, Cmd.none )
            else
                ( { model
                    | notice = Nothing
                  }
                , Cmd.none
                )

        -- FetchSuccess
        -- on success of 'fetch' set the response on the model
        -- if the reponse was an empty list,
        -- define a notice.
        -- set the isLoading state to False
        FetchSuccess (Ok results) ->
            case results of
                Nothing ->
                    ( { model
                        | results = Nothing
                        , isLoading = False
                        , notice = Nothing
                      }
                    , Cmd.none
                    )

                Just repos ->
                    if repos == [] then
                        ( { model
                            | results = Nothing
                            , isLoading = False
                            , notice = Just "No public repositories found for this user."
                          }
                        , Cmd.none
                        )
                    else
                        ( { model
                            | results = Just repos
                            , isLoading = False
                            , notice = Nothing
                          }
                        , Cmd.none
                        )

        -- FetchFail
        -- on Http fail set :
        -- the reponse as 'notice'
        -- the isLoading state to False
        -- result to Nothing
        FetchSuccess (Err err) ->
            ( { model
                | notice = Just (httpErrorString err)
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
                newElapsedTime =
                    elapsedTimeOf model.debounceState clockTime
            in
                if newElapsedTime > debounceTime then
                    ( { model
                        | debounceState = Nothing
                        , isLoading = False
                      }
                    , fetch (String.toLower (Maybe.withDefault "" model.query)) model.filter
                    )
                else
                    ( { model
                        | debounceState = Just { elapsedTime = newElapsedTime, prevClockTime = clockTime }
                        , isLoading = True
                      }
                    , Task.perform Tick Time.now
                    )



-- httpErrorString
-- Pattern match the error response.
-- A MASSIVE thanks to this article:
-- https://becoming-functional.com/http-error-checking-in-elm-fee8c4b68b7b#.6wz3kna56


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadUrl text ->
            "Bad Url: " ++ text

        Http.Timeout ->
            "Http Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus response ->
            "Bad Http Status: " ++ toString response.status.code

        Http.BadPayload message response ->
            "Bad Http Payload: "
                ++ toString message
                ++ " ("
                ++ toString response.status.code
                ++ ")"



-- fetch
-- take a query and a filter and perform a Http request


fetch : String -> String -> Cmd Msg
fetch query filter =
    let
        url =
            "https://api.github.com/users/" ++ query ++ "/repos?sort=" ++ filter

        request =
            Http.get url decodeRepos
    in
        Http.send FetchSuccess request



-- decodeRepos
-- return a decoded response we expect to receive


decodeRepos : Json.Decoder (Maybe (List Repo))
decodeRepos =
    let
        repo =
            Json.map7 Repo
                (Json.maybe (field "description" Json.string))
                (field "html_url" Json.string)
                (field "stargazers_count" Json.int)
                (field "watchers_count" Json.int)
                (field "full_name" Json.string)
                (field "owner" decodeAvatar)
                (field "owner" decodeLogin)
    in
        Json.maybe <| Json.list repo



-- decodeAvatar
-- pluck the avatar from 'owner' in the JSON response


decodeAvatar : Json.Decoder String
decodeAvatar =
    Json.at [ "avatar_url" ] Json.string



-- decodeLogin
-- pluck the login from 'owner' in the JSON response


decodeLogin : Json.Decoder String
decodeLogin =
    Json.at [ "login" ] Json.string
