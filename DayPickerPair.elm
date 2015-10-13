module DayPickerPair where

import DayPicker exposing (Model, Action, init, update, view)
import Html exposing (..)

type alias Model =
    { left: DayPicker.Model
    , right: DayPicker.Model
    }

init: (Int,Int,Int) -> Model
init (y,m,d) =
    let (ny,nm) =
        if m+1 > 12
            then (y+1,(m%12)+1)
            else (y,m)
    in { left = DayPicker.init (y,m,d)
       , right = DayPicker.init (ny,nm,d)
       }

type Action
    = Left DayPicker.Action
    | Right DayPicker.Action

update: Action -> Model -> Model
update action model =
    case action of
        Left action -> model    -- TODO
        Right action -> model   -- TODO

view: Signal.Address Action -> Model -> Html
view address model =
    div [] []
