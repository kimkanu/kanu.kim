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
    , greenBlue : Css.Color
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
    , greenBlue = hex "#77d4c1"
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
            , colorTheme.greenBlue
            , colorTheme.mint
            , colorTheme.lightBlue
            , colorTheme.blue
            , colorTheme.violet
            , colorTheme.pink
            , colorTheme.lightPink
            ]

        mod =
            modBy <| List.length colorList
    in
    List.Extra.getAt
        (hashString mod s |> (\i -> 5 * i + 8 |> mod))
        colorList
        |> Maybe.withDefault defaultColor


hashString : (Int -> Int) -> String -> Int
hashString mod =
    let
        hash =
            \a -> \b -> a + b
    in
    String.toList >> List.map Char.toCode >> List.foldl (hash << mod) 0 >> mod
