module Main exposing (..)

import Html
import Model exposing (model)
import View exposing (view)
import Update exposing (update)


-- main
-- kick out the jams!


main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
