module Page.Home exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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


view : { title : String, page : List (Html Msg) }
view =
    { title = "Home"
    , page =
        [ header [] [ h1 [] [ text "Homepage" ] ]
        , main_ [] viewPage
        ]
    }


viewPage : List (Html Msg)
viewPage =
    [ text "hi" ]
