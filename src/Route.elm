module Route exposing (Route(..), fromUrl, link, toUrl)

import Html.Styled exposing (Attribute, Html, a)
import Html.Styled.Attributes exposing (href)
import Url exposing (Url)
import Url.Parser exposing ((</>), Parser, s, string, top)



-- DEFINITIONS


type Route
    = Root
    | Shortener
    | Blog
    | BlogPost String
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
        , Url.Parser.map Blog <| s "blog"
        , Url.Parser.map BlogPost <| s "blog" </> string
        ]


toUrl : Route -> String
toUrl route =
    case route of
        Root ->
            "/"

        Shortener ->
            "/shortener"

        Blog ->
            "/blog"

        BlogPost slug ->
            "/blog/" ++ slug

        NotFound ->
            "/404"


{-| link
Type checked internal links.

  - Each path for each route is defined once
  - Easy to change path
  - You can't go to non-existing routes

-}
link : Route -> List (Attribute msg) -> List (Html msg) -> Html msg
link route attributes content =
    a (href (toUrl route) :: attributes) content
