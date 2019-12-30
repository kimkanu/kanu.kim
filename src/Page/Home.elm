module Page.Home exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Css exposing (alignItems, center, displayFlex, height, justifyContent, maxHeight, maxWidth, pct, px, vw, width)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)
import Http



-- MODEL


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}
    , Cmd.none
    )



-- UPDATE


type Msg
    = GotData (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotData data ->
            ( model, Cmd.none )



-- VIEW


view : Model -> { title : String, page : List (Html Msg) }
view _ =
    { title = "kanu.kim"
    , page =
        [ main_ [] viewMain
        ]
    }


viewMain : List (Html Msg)
viewMain =
    [ div
        [ css [ displayFlex, justifyContent center, alignItems center, height (pct 100) ] ]
        [ img
            [ src "https://avatars0.githubusercontent.com/u/22598138?s=460&v=4"
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
