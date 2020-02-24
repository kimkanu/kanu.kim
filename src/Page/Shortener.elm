port module Page.Shortener exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Css exposing (alignItems, backgroundColor, border, borderRadius, boxShadow5, calc, center, color, column, cursor, default, displayFlex, em, flexDirection, fontFamilies, fontSize, fontWeight, height, hex, hover, int, justifyContent, lineHeight, marginBottom, marginLeft, marginRight, maxWidth, minus, none, outline, padding2, pct, pointer, px, textAlign, vw, wait, width)
import Html.Styled exposing (Html, div, input, main_, span, text)
import Html.Styled.Attributes exposing (css, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import Util.Icon exposing (copy, ellipsis, icon, rightArrow)



-- MODEL


type alias Model =
    { url : String
    , shortened : ShortenedStatus
    }


type ShortenedStatus
    = NotShortened
    | Processing
    | Shortened
    | Empty


init : ( Model, Cmd Msg )
init =
    ( { url = "", shortened = Empty }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change String
    | ClickButton
    | ReceiveShortenedUrl (Result D.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newUrl ->
            ( { model
                | url = newUrl
                , shortened =
                    if newUrl == "" then
                        Empty

                    else if model.shortened == Processing then
                        Processing

                    else
                        NotShortened
              }
            , Cmd.none
            )

        ClickButton ->
            case model.shortened of
                NotShortened ->
                    ( { model | shortened = Processing }, shorten (E.string model.url) )

                Shortened ->
                    ( model, copyToClipboard (E.string model.url) )

                _ ->
                    ( model, Cmd.none )

        ReceiveShortenedUrl newUrl ->
            ( { model | shortened = Shortened, url = "kanu.kim/_" ++ Result.withDefault model.url newUrl }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    getShortenedUrl (decodeShortenedUrl >> ReceiveShortenedUrl)


decodeShortenedUrl : D.Value -> Result D.Error String
decodeShortenedUrl =
    D.decodeValue D.string



-- VIEW


view : Model -> { title : String, page : List (Html Msg) }
view model =
    { title = "kanu.kim URL Shortener"
    , page =
        [ main_ [] <| viewContent model
        ]
    }


viewContent : Model -> List (Html Msg)
viewContent model =
    [ div
        [ css
            [ displayFlex
            , flexDirection column
            , justifyContent center
            , alignItems center
            , height (pct 100)
            , fontFamilies [ "Mosk" ]
            ]
        ]
      <|
        List.concat
            [ viewTitle
            , viewInput model
            ]
    ]


viewTitle : List (Html Msg)
viewTitle =
    [ div
        [ css
            [ fontSize (em 1.7)
            , color (hex "424242")
            , marginBottom (em 0.8)
            , marginRight (em 2.4)
            ]
        ]
        [ span [ css [ fontWeight (int 900) ] ] [ text "URL " ], text "SHORTENER" ]
    ]


viewInput : Model -> List (Html Msg)
viewInput model =
    [ div
        [ css [ displayFlex ] ]
        [ input
            [ type_ "text"
            , onInput Change
            , value model.url
            , css
                [ fontFamilies [ "Mosk" ]
                , fontSize (em 2.1)
                , border (px 0)
                , outline none
                , backgroundColor <|
                    if model.shortened == Shortened then
                        hex "cce7ea"

                    else
                        hex "E4ECF0"
                , padding2 (em 0.5) (em 0.8)
                , borderRadius (em 1.2)
                , width (em 18)
                , maxWidth (calc (vw 100) minus (em 4))
                , color (hex "40494a")
                ]
            ]
            []
        , div
            [ onClick ClickButton
            , css
                [ width (em 1.29)
                , height (em 1.29)
                , lineHeight (em 1.29)
                , fontSize (em 3.5)
                , backgroundColor
                    (hex <|
                        if model.shortened == Processing || model.shortened == Empty then
                            "828788"

                        else
                            "20BACA"
                    )
                , borderRadius (pct 50)
                , color (hex "ffffff")
                , textAlign center
                , marginLeft (em 0.3)
                , cursor <|
                    if model.shortened == Processing then
                        wait

                    else if model.shortened == Empty then
                        default

                    else
                        pointer
                , hover <|
                    if model.shortened == Processing || model.shortened == Empty then
                        []

                    else
                        [ boxShadow5 (px 0) (px 8) (px 18) (px -4) (hex "14b8ca") ]
                ]
            ]
            [ case model.shortened of
                Processing ->
                    icon [] ellipsis

                Shortened ->
                    icon [ fontSize (em 0.72) ] copy

                _ ->
                    icon [] rightArrow
            ]
        ]
    ]


port shorten : E.Value -> Cmd msg


port copyToClipboard : E.Value -> Cmd msg


port getShortenedUrl : (D.Value -> msg) -> Sub msg
