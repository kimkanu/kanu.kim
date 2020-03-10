module Page.Home exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Css exposing (alignItems, center, displayFlex, height, justifyContent, maxHeight, maxWidth, pct, px, vw, width)
import Html.Styled exposing (Html, div, img, main_)
import Html.Styled.Attributes exposing (css, src)



-- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}
    , Cmd.none
    )



-- UPDATE


type alias Msg =
    ()


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )



-- VIEW


view : Model -> { title : String, page : List (Html Msg) }
view _ =
    { title = "kanu.kim"
    , page =
        [ main_ [ css [ height (pct 100) ] ] viewMain
        ]
    }


viewMain : List (Html Msg)
viewMain =
    [ div
        [ css [ displayFlex, justifyContent center, alignItems center, height (pct 100) ] ]
        [ img
            [ src "https://avatars0.githubusercontent.com/u/22598138"
            , css
                [ width (vw 50)
                , maxWidth (px 280)
                , height (vw 50)
                , maxHeight (px 280)
                ]
            ]
            []
        ]
    ]
