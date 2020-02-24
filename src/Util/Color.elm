module Util.Color exposing (colorTheme, getColorFromString)

import Css exposing (hex)
import List.Extra


type alias ColorTheme =
    { boldText : Css.Color
    , text : Css.Color
    , ambientText : Css.Color
    , lightGray : Css.Color
    , ambientGray : Css.Color
    , red : Css.Color
    , redOrange : Css.Color
    , orange : Css.Color
    , melon : Css.Color
    , mint : Css.Color
    , lightBlue : Css.Color
    , blue : Css.Color
    , violet : Css.Color
    , pink : Css.Color
    , lightPink : Css.Color
    }


colorTheme : ColorTheme
colorTheme =
    { boldText = hex "#424242"
    , text = hex "#3a3a3c"
    , ambientText = hex "#657786"
    , lightGray = hex "#929da6"
    , ambientGray = hex "#d5d9dd"
    , red = hex "#E45B79"
    , redOrange = hex "#FF7372"
    , orange = hex "#FF9276"
    , melon = hex "#E4E2B9"
    , mint = hex "#6AC3C9"
    , lightBlue = hex "#4a7ddb"
    , blue = hex "#1656cf"
    , violet = hex "#7561ec"
    , pink = hex "#d583e0"
    , lightPink = hex "#ea9cd7"
    }


getColorFromString : String -> Css.Color
getColorFromString s =
    let
        defaultColor =
            colorTheme.lightGray

        colorList =
            [ colorTheme.lightGray
            , colorTheme.ambientText
            , colorTheme.red
            , colorTheme.redOrange
            , colorTheme.orange
            , colorTheme.melon
            , colorTheme.mint
            , colorTheme.lightBlue
            , colorTheme.blue
            , colorTheme.violet
            , colorTheme.pink
            , colorTheme.lightPink
            ]
    in
    List.Extra.getAt
        (hashString s |> modBy (List.length colorList))
        colorList
        |> Maybe.withDefault defaultColor


hashString : String -> Int
hashString =
    String.toList >> List.map Char.toCode >> List.foldl (+) 0
