module DateUtils where

formatWeekdayLong: String -> Int -> String
formatWeekdayLong locale wd =
    case wd of
        1 -> "Monday"
        2 -> "Tuesday"
        3 -> "Wednesday"
        4 -> "Thursday"
        5 -> "Friday"
        6 -> "Saturday"
        7 -> "Sunday"
        x -> "invalid weekday: " ++ toString x

getFirstDayOfWeek: String -> Int
getFirstDayOfWeek locale =
    0

formatMonth: Int -> String
formatMonth month =
    case month of
        1 -> "January"
        2 -> "February"
        3 -> "March"
        4 -> "April"
        5 -> "May"
        6 -> "June"
        7 -> "Jule"
        8 -> "August"
        9 -> "September"
        10 -> "October"
        11 -> "November"
        12 -> "December"
        x -> "invalid month: " ++ toString x
