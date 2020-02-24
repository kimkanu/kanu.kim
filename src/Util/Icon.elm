module Util.Icon exposing (Icon, backArrow, calendar, caution, copy, ellipsis, icon, rightArrow)

import Css exposing (Style, fontFamilies)
import Html.Styled exposing (Html, span, text)
import Html.Styled.Attributes exposing (css)


type Icon
    = Calendar
    | Ellipsis
    | Copy
    | RightArrow
    | BackArrow
    | Caution


icon : List Style -> Icon -> Html msg
icon c i =
    span [ css <| [ fontFamilies [ "BoxIcons" ] ] ++ c ]
        [ case i of
            Calendar ->
                text "\u{E9E5}"

            Ellipsis ->
                text "\u{EA7F}"

            Copy ->
                text "\u{EA51}"

            RightArrow ->
                text "\u{EBAB}"

            BackArrow ->
                text "\u{E988}"

            Caution ->
                text "\u{EB4E}"
        ]


calendar : Icon
calendar =
    Calendar


ellipsis : Icon
ellipsis =
    Ellipsis


copy : Icon
copy =
    Copy


rightArrow : Icon
rightArrow =
    RightArrow


backArrow : Icon
backArrow =
    BackArrow


caution : Icon
caution =
    Caution
