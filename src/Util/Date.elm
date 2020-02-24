module Util.Date exposing (dateToString, maybeDateToString)

import Date exposing (Date)


dateToString : Date -> String
dateToString =
    Date.format "MMM ddd, y"


maybeDateToString : Maybe Date -> String
maybeDateToString =
    Maybe.withDefault "Someday" << Maybe.map dateToString
