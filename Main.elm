module Main (..) where

import StartApp.Simple
import Html exposing (Html)
import DayPicker exposing (..)


main : Signal Html
main =
  StartApp.Simple.start
    { model = init ( 2015, 1, 1 )
    , update = update
    , view = view
    }
