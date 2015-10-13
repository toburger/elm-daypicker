import Html exposing (..)
import Html.Events exposing (onClick)
import StartApp.Simple exposing (start)
import Debug

type alias Model = Int

type Action
    = Increment
    | Decrement

init: Int -> Model
init model = model

update: Action -> Model -> Model
update action model =
    case action of
        Increment -> model + 1
        Decrement -> model - 1
    |> Debug.watch "model"

view: Signal.Address Action -> Model -> Html
view address model =
    div []
        [ button [ onClick address Decrement ] [ text "-" ]
        , text (toString model)
        , button [ onClick address Increment ] [ text "+" ]
        ]

main: Signal Html
main =
    start
        { model = init 0
        , update = update
        , view = view
        }
