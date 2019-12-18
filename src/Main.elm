module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Css exposing (alignItems, center, displayFlex, height, justifyContent, maxHeight, maxWidth, pct, px, vh, vw, width)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, href, src)
import Html.Styled.Events exposing (onClick)
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "URL Interceptor"
    , body =
        List.map toUnstyled viewMain
    }


viewMain : List (Html msg)
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
