module Page.PostCategory exposing (viewCategory, viewCategoryItem)

import Css exposing (backgroundColor, borderRadius, color, display, height, marginRight, none, px, width)
import Html.Styled exposing (Html, a, span, text)
import Html.Styled.Attributes exposing (css, href)
import Url
import Util.Color exposing (colorTheme, getColorFromString)


viewCategory : List String -> List (Html msg)
viewCategory =
    let
        middleDot =
            span [ css [ Css.margin2 (px 0) (px 4) ] ] [ text " · " ]
    in
    List.map viewCategoryItem >> List.intersperse middleDot


viewCategoryItem : String -> Html msg
viewCategoryItem category =
    a
        [ href <| "/blog?category=" ++ Url.percentEncode category
        , css [ Css.textDecoration Css.none, color colorTheme.ambientText, Css.whiteSpace Css.noWrap ]
        ]
        [ span [ css [ backgroundColor (getColorFromString category), display Css.inlineBlock, width (px 14), height (px 14), borderRadius (px 8), Css.transform <| Css.translateY (px 2), marginRight (px 4) ] ] []
        , text category
        ]
