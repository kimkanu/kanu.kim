port module Page.BlogList exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Css exposing (alignItems, backgroundColor, bold, borderRadius, calc, center, color, column, display, displayFlex, em, flexDirection, fontFamilies, fontSize, fontWeight, height, hex, int, justifyContent, lineHeight, marginBottom, marginLeft, marginRight, maxWidth, minus, none, padding2, padding3, pct, px, textAlign, vh, width)
import Date exposing (Date)
import Html.Styled exposing (Html, a, br, div, h1, img, main_, p, pre, span, text)
import Html.Styled.Attributes exposing (css, href, src)
import Http
import Json.Decode as D
import Markdown
import Url
import Util.Color exposing (colorTheme, getColorFromString)
import Util.Date exposing (maybeDateToString)
import Util.Icon exposing (calendar, icon)
import Util.Logo exposing (smallLogoImage)



-- MODEL


type Model
    = Loading (Maybe String)
    | LoadingFailure (Maybe String)
    | JsonFailure (Maybe String) String
    | Success (Maybe String) SuccessModel


type alias SuccessModel =
    List PostData


type alias PostData =
    { slug : String
    , title : String
    , date : Maybe Date
    , category : List String
    , summary : String
    }


init : Maybe String -> ( Model, Cmd Msg )
init maybeCategory =
    ( Loading maybeCategory
    , Http.get
        { url =
            "https://raw.githubusercontent.com/kimkanu/kanu.kim/gh-pages/posts/index"
        , expect = Http.expectString FetchIndex
        }
    )



-- UPDATE


type Msg
    = FetchIndex (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchIndex response ->
            let
                category =
                    getCategory model
            in
            case response of
                Err _ ->
                    ( LoadingFailure category, Cmd.none )

                Ok indexJson ->
                    let
                        entryDecoder =
                            D.map5 PostData
                                (D.field "slug" D.string)
                                (D.field "title" D.string)
                                (D.field "date" <|
                                    D.map (Date.fromIsoString >> Result.toMaybe) D.string
                                )
                                (D.field "category" <| D.list D.string)
                                (D.field "summary" D.string)

                        decoder =
                            D.list entryDecoder

                        decodedJson =
                            D.decodeString decoder indexJson
                    in
                    case decodedJson of
                        Err err ->
                            ( JsonFailure (getCategory model) (D.errorToString err)
                            , Cmd.none
                            )

                        Ok postList ->
                            ( Success (getCategory model) postList
                            , highlightSyntaxList ()
                            )


getCategory : Model -> Maybe String
getCategory m =
    case m of
        Loading q ->
            q

        LoadingFailure q ->
            q

        JsonFailure q _ ->
            q

        Success q _ ->
            q



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> { title : String, page : List (Html Msg) }
view model =
    case model of
        Success maybeCategory m ->
            { title = "Blog - kanu.kim"
            , page =
                [ main_
                    [ css [ height (pct 100), fontSize (em 1.16), color colorTheme.text ] ]
                  <|
                    [ div [ css [ displayFlex, flexDirection column, alignItems center ] ] <|
                        (++) viewHeader <|
                            [ div [ css [ width (pct 100), maxWidth (px 720), Css.marginTop (px 32), marginBottom (px 72) ] ] <|
                                let
                                    renderPostList =
                                        List.map viewPost
                                            >> List.intersperse viewPostSeparator

                                    viewHeaderCategory =
                                        \category ->
                                            [ div [ css [ textAlign center, fontFamilies [ "Mosk" ], fontSize (em 1.3), marginBottom (em 2) ] ]
                                                [ span [ css [ Css.paddingRight (em 0.4), color colorTheme.text ] ] [ text "Category:" ]
                                                , span [ css [ fontWeight (int 700), color colorTheme.boldText ] ] [ text category ]
                                                ]
                                            , viewPostSeparator
                                            ]

                                    appendCategory =
                                        Maybe.map viewHeaderCategory maybeCategory
                                            |> Maybe.withDefault []
                                            |> (++)

                                    dealEmpty l =
                                        if List.length l == 0 then viewEmpty else l
                                in
                                case maybeCategory of
                                    Nothing ->
                                        m |> renderPostList |> dealEmpty

                                    Just category ->
                                        m
                                            |> List.filter (.category >> List.member category)
                                            |> renderPostList
                                            |> dealEmpty
                                            |> appendCategory
                            ]
                    ]
                ]
            }

        JsonFailure _ e ->
            { title = "Blog - kanu.kim"
            , page =
                [ main_ [ css [ height (pct 100), fontSize (em 1.16), color colorTheme.text ] ] <|
                    viewHeader
                        ++ [ text e ]
                ]
            }

        Loading _ ->
            { title = "Blog - kanu.kim"
            , page =
                [ main_ [ css [ height (pct 100), fontSize (em 1.16), color colorTheme.text ] ] <|
                    viewLoading
                ]
            }

        LoadingFailure maybeCategory ->
            { title = "Blog - kanu.kim"
            , page =
                [ main_ [] <|
                    viewHeader
                        ++ [ text <| Maybe.withDefault "" maybeCategory ]
                ]
            }


viewHeader : List (Html Msg)
viewHeader =
    [ div [ css [ height (px 80), displayFlex, justifyContent center, alignItems center ] ]
        [ div [ css [ width (px 56) ] ]
            [ a [ href "/" ]
                [ img [ src smallLogoImage, css [ width (px 56) ] ] [] ]
            ]
        , div [ css [ textAlign center, fontSize (em 1.5), marginLeft (px 24) ] ]
            [ a [ href "/blog", css [ Css.textDecoration Css.none, color colorTheme.text ] ]
                [ span [ css [ fontWeight (int 700), fontFamilies [ "Metropolis" ], color colorTheme.boldText ] ] [ text "blog://" ]
                , span [ css [ fontFamilies [ "Mosk" ], marginLeft (px 2) ] ] [ text "kanu.kim" ]
                ]
            ]
        ]
    ]


viewPost : PostData -> Html Msg
viewPost postData =
    div [ css [ width (pct 100), Css.boxSizing Css.borderBox, padding2 (px 0) (px 32), fontFamilies [ "Mosk" ], marginBottom (px 32) ] ] <|
        [ Html.Styled.h2 [ css [ marginBottom (px 0), fontWeight (int 900), fontFamilies [ "Regattia" ], fontSize (em 1.8), lineHeight (em 1) ] ]
            [ a [ href <| "/blog/" ++ Url.percentEncode postData.slug, css [ Css.textDecoration Css.none, color colorTheme.boldText ] ] [ text postData.title ] ]
        , span [ css [ fontSize (em 0.85), color colorTheme.ambientText, lineHeight (em 2.5) ] ] <|
            [ icon [] calendar
            , text <| " " ++ maybeDateToString postData.date
            , span [ css [ padding2 (px 0) (px 4) ] ] [ text " | " ]
            ] ++ viewCategory postData.category
        ]
            ++ (Markdown.toHtml Nothing postData.summary |> List.map Html.Styled.fromUnstyled)


viewPostSeparator : Html Msg
viewPostSeparator =
    div
        [ css
            [ backgroundColor colorTheme.ambientGray
            , height (px 1)
            , Css.margin2 (px 16) (px 0)
            ]
        ]
        []


viewCategory : List String -> List (Html Msg)
viewCategory =
    \a ->
        Maybe.withDefault [] <|
            List.tail <|
                List.concatMap viewCategoryItem a


viewCategoryItem : String -> List (Html Msg)
viewCategoryItem category =
    [ span [ css [ Css.margin2 (px 0) (px 4) ] ] [ text " · " ]
    , a
        [ href <| "/blog?category=" ++ Url.percentEncode category
        , css [ Css.textDecoration Css.none, color colorTheme.ambientText ]
        ]
        [ span [ css [ backgroundColor (getColorFromString category), display Css.inlineBlock, width (px 14), height (px 14), borderRadius (px 8), Css.transform <| Css.translateY (px 2), marginRight (px 4) ] ] []
        , text category
        ]
    ]


viewEmpty : List (Html Msg)
viewEmpty =
    [ div [ css [ width (pct 100), textAlign center, padding2 (em 3) (px 0), fontFamilies [ "Mosk" ], color colorTheme.text ] ] [ span [] [ text "Empty!" ] ] ]


viewLoading : List (Html Msg)
viewLoading =
    [ div
        [ css [ height (pct 100), displayFlex, flexDirection column, justifyContent center, alignItems center ] ]
        [ div [ css [ width (px 112) ] ]
            [ img [ src smallLogoImage, css [ width (px 112) ] ] [] ]
        , div [ css [ textAlign center, fontFamilies [ "Mosk" ] ] ]
            [ span [] [ text "Loading..." ]
            ]
        ]
    ]


viewJsonError : String -> List (Html Msg)
viewJsonError error =
    [ div [ css [ displayFlex, justifyContent center, alignItems center, height (calc (vh 100) minus (px 80)) ] ]
        [ div [ css [ width (pct 100), maxWidth (px 400), Css.boxSizing Css.borderBox, displayFlex, flexDirection column, alignItems center, textAlign center ] ] <|
            [ h1
                [ css
                    [ fontFamilies [ "BoxIcons" ]
                    , fontWeight (int 900)
                    , fontSize (em 8)
                    , textAlign center
                    , lineHeight (em 1)
                    , Css.margin3 (px 0) (px 0) (px 32)
                    , color colorTheme.lightGray
                    ]
                ]
                [ text "\u{EB4E}" ]
            , p []
                [ span [ css [ fontFamilies [ "Mosk" ], color colorTheme.text ] ]
                    [ text <| "This page made the following error while parsing the document:"
                    , pre [ css [ backgroundColor (hex "F5F5F7"), padding2 (px 2) (px 4), borderRadius (px 4), color colorTheme.red, display Css.inlineBlock ] ] [ text error ]
                    ]
                ]
            , p []
                [ a [ href "/blog" ]
                    [ span [ css [ display Css.inlineBlock, fontFamilies [ "BoxIcons" ], color colorTheme.text, Css.transform <| Css.translateY (px 1) ] ]
                        [ text <| "\u{E988}"
                        ]
                    , span [ css [ fontFamilies [ "Mosk" ], color colorTheme.text ] ]
                        [ text <| " Return to the list"
                        ]
                    ]
                ]
            ]
        ]
    ]



-- Decapitalize a string except the first character


decapitalize : String -> String
decapitalize s =
    let
        concatCharString =
            \t ->
                let
                    ( c, r ) =
                        t
                in
                String.fromChar c ++ r
    in
    String.uncons s
        |> Maybe.map (Tuple.mapSecond String.toLower >> concatCharString)
        |> Maybe.withDefault ""


port highlightSyntaxList : () -> Cmd msg
