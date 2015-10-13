module DayPicker where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Debug
import DateUtils

type Keys
    = Left
    | Right
    | Enter
    | Space

type alias Model =
    { selectedDate: (Int,Int,Int)
    , tabIndex: Int
    , initialNewMonth: Int
    , numberOfMonths: Int
    , locale: String
    }

init: (Int, Int, Int) -> Model
init (year, month, day) =
    { selectedDate = (year,month,day)
    , tabIndex = 0
    , initialNewMonth = month
    , numberOfMonths = 2
    , locale = "en"
    }

type Action
    = NextMonth
    | PreviousMonth
    | SelectYear Int
    | SelectMonth (Int,Int)
    | SelectDay (Int,Int,Int)
    | SelectDays List(Int,Int,Int)

type alias ActionSignal = Signal.Address Action

update: Action -> Model -> Model
update action model =
    case action of
        NextMonth ->
            let (year,month,day) = model.selectedDate
            in { model |
                selectedDate <- (year,month+1,day) }
        PreviousMonth ->
            let (year,month,day) = model.selectedDate
            in { model |
                selectedDate <- (year,month-1,day) }
        SelectYear year ->
            let (_,month,day) = model.selectedDate
            in { model |
                selectedDate <- (year,month,day) }
        SelectMonth (year,month) ->
            let (_,_,day) = model.selectedDate
            in { model
                | selectedDate <- (year,month,day) }
        SelectDay (year,month,day) ->
            { model
                | selectedDate <- (year,month,day) }
    |> Debug.watch "model"

viewMonths: ActionSignal -> Model -> Int -> List Html
viewMonths address model year =
    --model
    [model.initialNewMonth..(model.initialNewMonth+model.numberOfMonths)]
    |> List.indexedMap (viewMonth address model year)

viewMonth: ActionSignal -> Model -> Int -> Int -> Int -> Html
viewMonth address model year key month =
    div
        [ class "DayPicker-Month"
        , monthStyle
        , onClick address (SelectMonth (year,month))
        , attribute "key" (toString i)
        ]
        [ div [ class "DayPicker-Caption"
              , monthCaptionStyle
              , onClick address (SelectMonth (year,month))
              ]
            [ text (DateUtils.formatMonth month) ]
        , div [ class "DayPicker-Weekdays" ]
            [ viewWeekDays address model ]
        , div [ class "DayPicker-Body" ]
            [ viewWeeksInMonth address model year month ]
        ]

viewWeekDays: ActionSignal -> Model -> Html
viewWeekDays address { locale } =
    [1..7]
    |> List.map (\i ->
        div
            [ class "DayPicker-Weekday"
            , weekdayStyle
            , attribute "key" (toString i)]
            [ div
                [ title (DateUtils.formatWeekdayLong locale i) ]
                [ text (DateUtils.formatWeekdayLong locale i) ] ])
    |> div []

getWeekList: Int -> Int -> List (List Int)
getWeekList month firstDayOfWeek =
    [[1..7],[8..14],[15..21],[22..28],[29..31]]

renderDay: Int -> Html
renderDay day =
    text (toString day)

viewDay: ActionSignal -> Model -> Int -> Int -> Int -> Html
viewDay address model year month day =
    let key = toString year ++ toString month ++ toString day
        tabIndex = model.tabIndex -- TODO
        className = "DayPicker-Day"
        isActive = model.selectedDate == (year,month,day)
    in div
        [ attribute "key" (toString key)
        , dayStyle isActive
        , attribute "tabIndex" (toString tabIndex)
        , onClick address (SelectDay (year,month,day))
        , class className
        , attribute "role" "cell"
        ]
        [ renderDay day ]

isActiveMonth: (Int,Int,Int) -> Int -> Int -> Bool
isActiveMonth (y,m,_) year month =
    y == year && m == month

viewWeeksInMonth: ActionSignal -> Model -> Int -> Int -> Html
viewWeeksInMonth address model year month =
    let firstDayOfWeek =
        DateUtils.getFirstDayOfWeek model.locale
    in getWeekList month firstDayOfWeek
       |> List.indexedMap (\i weekdays ->
        div [ attribute "key" (toString i)
            , class "DayPicker-Week"
            , weekStyle (isActiveMonth model.selectedDate year month)
            , attribute "role" "row"
            ]
            ( List.map (viewDay address model year month) weekdays) )
       |> div []

view: ActionSignal -> Model -> Html
view address model =
    div [ class "className"
        , style []
        , attribute "role" "widget"
        , attribute "tabIndex" (toString model.tabIndex)
        ]
        ( viewMonths address model 2015 )

monthStyle: Html.Attribute
monthStyle =
    style
        [ ("border", "solid 1px black")
        , ("margin", "5px")
        ]

monthCaptionStyle: Html.Attribute
monthCaptionStyle =
    style
        [ ("cursor", "pointer")
        , ("font-weight", "bold")
        ]

weekdayStyle: Html.Attribute
weekdayStyle =
    style
        [ ("display", "inline-block")
        , ("width", "30px")
        , ("overflow", "hidden")
        ]

weekStyle: Bool -> Html.Attribute
weekStyle active =
    style
        [ ("background-color", if active then "#DADADA" else "inherit")
        ]

dayStyle: Bool -> Html.Attribute
dayStyle active =
    style
        [ ("display", "inline-block")
        , ("width", "30px")
        , ("height", "30px")
        , ("text-align", "center")
        , ("vertical-align", "middle")
        , ("cursor", "pointer")
        , ("background-color", if active then "#0044FF" else "inherit")
        , ("color", if active then "white" else "black")
        ]
