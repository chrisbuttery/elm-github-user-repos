module Styles where

import Html exposing (..)
import Html.Attributes as Attr exposing (..)

container : Attribute
container =
  style
    [ ("font-size", "1em")
    , ("margin", "0 auto")
    , ("width", "70%")
    ]

header : Attribute
header =
  style
    [ ("color", "#333")
    , ("font-size", "1.4em")
    , ("margin", "20px 0")
    ]

input : Attribute
input =
  style
    [ ("border", "1px solid #ccc")
    , ("font-size", "1em")
    , ("border-radius", "2px")
    , ("box-sizing", "border-box")
    , ("color", "#666")
    , ("height", "50px")
    , ("margin", "0 0 10px 0")
    , ("padding", "10px")
    , ("width", "100%")
    ]

message : Attribute
message =
  style
  [ ("background-color", "#fafafa")
  , ("border", "1px solid #e5e5e5")
  , ("border-radius", "2px")
  , ("font-size", "1.2em")
  , ("margin", "20px 0 0 0")
  , ("padding", "30px")
  , ("text-align", "center")
  ]

results : Attribute
results =
  style
    [ ("font-size", "1em")
    , ("padding", "0")
    , ("width", "100%")
    ]

result : Attribute
result =
  style
    [ ("border-top", "1px solid #ddd")
    , ("color", "#767676")
    , ("font-size", "1em")
    , ("list-style-type", "none")
    , ("margin", "10px 0 0 0")
    , ("padding", "20px 10px")
    ]

copy : Attribute
copy =
  style
    [ ("overflow", "hidden")]

meta : Attribute
meta =
  style
    [ ("float", "right")
    , ("font-size", "0.8em")
    , ("font-weight", "bold")
    ]

name : Attribute
name =
  style
    [ ("color", "#4078c0")
    , ("display", "block")
    , ("font-size", "1.2em")
    , ("margin-bottom", "10px")]

hidden : Attribute
hidden =
  style
    [ ("display", "none")]