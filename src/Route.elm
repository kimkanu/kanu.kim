module Route exposing (Route(..), fromUrl, link, toTitle, toUrl)

import Html.Styled exposing (Attribute, Html, a)
import Html.Styled.Attributes exposing (href)
import Url exposing (Url)
import Url.Builder as Builder
import Url.Parser exposing (Parser, s, top)



-- DEFINITIONS


type Route
    = Root
    | Shortener
    | NotFound



-- PARSE


fromUrl : Url -> Route
fromUrl =
    Maybe.withDefault NotFound
        << Url.Parser.parse parser


parser : Parser (Route -> a) a
parser =
    Url.Parser.oneOf
        [ Url.Parser.map Root top
        , Url.Parser.map Shortener <| s "shortener"
        ]



-- CONVERSIONS


type alias RouteData =
    { url : String
    , title : String
    }


toRouteData : Route -> RouteData
toRouteData route =
    case route of
        Root ->
            RouteData "/" "kanu.kim"

        Shortener ->
            RouteData "/shortener" "kanu.kim URL Shortener"

        NotFound ->
            RouteData "/404" "Not Found"


toTitle : Route -> String
toTitle =
    .title << toRouteData


toUrl : Route -> String
toUrl =
    .url << toRouteData


{-| link
Type checked internal links.

  - Each path for each route is defined once
  - Easy to change path
  - You can't go to non-existing routes

-}
link : Route -> List (Attribute msg) -> List (Html msg) -> Html msg
link route attributes content =
    a (href (toUrl route) :: attributes) content
