module View exposing (view)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Update exposing (Msg(..))
import Model exposing (Model, Repo)
import Loader


-- onInput
-- on input of the text field,
-- convert the value to a 'Maybe' and
-- pass to Input


onInput : Attribute Msg
onInput =
    on "input"
        (Json.map Input
            (Json.maybe <| targetValue)
        )



-- onChecked
-- on 'check' of a radio button
-- pass it's value to SortBy


onChecked : Attribute Msg
onChecked =
    on "change"
        (Json.map SortBy
            targetValue
        )



-- renderLoadingState
-- rendering a loading state


renderLoadingState : Bool -> Html Msg
renderLoadingState isLoading =
    div
        [ classList
            [ ( "is-loading", True )
            , ( "hidden", isLoading == False )
            ]
        ]
        [ Loader.render ]



-- renderProfile
-- render a profile of the user


renderProfile : Repo -> Html Msg
renderProfile item =
    div [ class "profile" ]
        [ div [ class "profile__image" ]
            [ img [ class "avatar", src item.avatar_url ] []
            ]
        , a [ class "username", href ("https://github.com/" ++ item.login) ] [ text item.login ]
        ]



-- renderHeader
-- render the header


renderHeader : Html Msg
renderHeader =
    div [ class "header" ]
        [ h1 [ class "app-title" ]
            [ text ("GitHub User Repos") ]
        ]



-- renderRepo
-- render a repo item


renderRepo : Repo -> Html Msg
renderRepo item =
    div [ class "repo" ]
        [ div [ class "meta" ]
            [ div [ class "meta--col" ]
                [ div [ class "meta__data" ]
                    [ span [ class "meta__count" ] [ text (toString item.stargazers_count) ]
                    , span [ class "meta__title" ] [ text "Stars" ]
                    ]
                ]
            , div [ class "meta--col" ]
                [ div [ class "meta__data" ]
                    [ span [ class "meta__count" ] [ text (toString item.watchers_count) ]
                    , span [ class "meta__title" ] [ text "Watchers" ]
                    ]
                ]
            ]
        , div [ class "copy" ]
            [ a
                [ href item.html_url
                , title item.full_name
                , class "name"
                ]
                [ text item.full_name ]
            , div [] [ text (Maybe.withDefault "" item.description) ]
            ]
        ]



-- renderRepos
-- render a collection of repos


renderRepos : Model -> Html Msg
renderRepos model =
    div [ class "repos" ]
        (List.map renderRepo (Maybe.withDefault [] model.results))



-- filterOptions
-- define a list of options to filter repos


filterOptions : List String
filterOptions =
    [ "full_name"
    , "created"
    , "updated"
    , "pushed"
    ]



-- renderRadioButton
-- render a radio button element


renderRadioButton : String -> Model -> Html Msg
renderRadioButton name model =
    let
        labelText =
            if name == "full_name" then
                "default"
            else
                name
    in
        label [ class "label filter__option-label" ]
            [ input
                [ type_ "radio"
                , class "filter__option-input"
                , value name
                , checked (model.filter == name)
                , onChecked
                ]
                []
            , text labelText
            ]



-- renderRadioButtons
-- render a collection of radio buttons


renderRadioButtons : Model -> Html Msg
renderRadioButtons model =
    div [ class "filter" ]
        [ span [ class "label filter__label" ]
            [ text "Sort repositories by: " ]
        , div [ class "filter__options" ]
            (List.map
                (\name ->
                    renderRadioButton name model
                )
                filterOptions
            )
        ]



-- renderForm
-- render the form


renderForm : Model -> Html Msg
renderForm model =
    div [ class "form" ]
        [ input
            [ placeholder "Type in a Github username to look up"
            , value (Maybe.withDefault "" model.query)
            , onInput
            , class "input"
            , id "search"
            ]
            []
        , renderNotice model
        , renderRadioButtons model
        ]



-- renderNotice
-- render a notice message for errors or other messages


renderNotice : Model -> Html Msg
renderNotice model =
    let
        notice =
            model.notice

        visibility =
            if notice == Nothing then
                True
            else
                False
    in
        div
            [ classList
                [ ( "notice", True )
                , ( "hidden", visibility )
                ]
            ]
            [ span [ class "close", onClick HideNotice ] []
            , span [ class "smiley" ] [ text ":(" ]
            , span [ class "notice__text" ] [ text (Maybe.withDefault "" notice) ]
            ]



-- view
-- show the things


view : Model -> Html Msg
view model =
    let
        first =
            List.take 1 (Maybe.withDefault [] model.results)
    in
        div [ class "container" ]
            [ renderHeader
            , renderForm model
            , div [ class "body" ]
                [ renderLoadingState model.isLoading
                , div [] (List.map (\val -> renderProfile val) first)
                , renderRepos model
                ]
            ]
