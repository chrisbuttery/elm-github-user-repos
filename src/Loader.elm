module Loader exposing (..)

import Html exposing (Html)
import Svg exposing (Svg, svg, circle, animate)
import Svg.Attributes exposing (..)


animateElement : String -> Svg msg
animateElement delay =
  animate
    [ attributeName "fill-opacity"
    , begin delay
    , dur "1s"
    , values "1;.2;1"
    , calcMode "linear"
    , repeatCount "indefinite"
    ] []


render : Html msg
render =
  svg
    [ class "foo"
    , width "105"
    , height "105"
    , viewBox "0 0 105 105"
    , fill "#fff"
    ] [
      circle [ cx "12.5", cy "12.5", r "12.5" ] [
        animateElement "0s"
      ]
      , circle [ cx "12.5", cy "52.5", r "12.5" ] [
        animateElement "100ms"
      ]
      , circle [ cx "92.5", cy "52.5", r "12.5" ] [
        animateElement "300ms"
      ]
      , circle [ cx "52.5", cy "52.5", r "12.5" ] [
        animateElement "600ms"
      ]
      , circle [ cx "92.5", cy "12.5", r "12.5" ] [
        animateElement "800ms"
      ]
      , circle [ cx "52.5", cy "12.5", r "12.5" ] [
        animateElement "400ms"
      ]
      , circle [ cx "12.5", cy "92.5", r "12.5" ] [
        animateElement "700ms"
      ]
      , circle [ cx "52.5", cy "92.5", r "12.5" ] [
        animateElement "500ms"
      ]
      , circle [ cx "92.5", cy "92.5", r "12.5" ] [
        animateElement "200ms"
      ]
    ]
