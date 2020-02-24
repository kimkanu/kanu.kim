port module Page.BlogPost exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Css exposing (alignItems, backgroundColor, bold, borderRadius, calc, center, color, column, display, displayFlex, em, flexDirection, fontFamilies, fontSize, fontWeight, height, hex, int, justifyContent, lineHeight, marginBottom, marginLeft, marginRight, maxWidth, minus, none, padding2, padding3, pct, px, textAlign, vh, width)
import Css.Media as Media exposing (only, screen, withMedia)
import Date exposing (Date)
import Html.Styled exposing (Html, a, div, h1, img, main_, p, pre, span, text)
import Html.Styled.Attributes exposing (css, href, src)
import Http
import Markdown
import Page.PostCategory exposing (viewCategory)
import Url
import Util.Color exposing (colorTheme)
import Util.Date exposing (maybeDateToString)
import Util.Icon exposing (backArrow, calendar, caution, icon)
import Util.Logo exposing (smallLogoImage)
import Yaml.Decode



-- MODEL


type Model
    = Loading String
    | LoadingFailure String
    | YamlFailure
        { slug : String
        , raw : String
        , error : String
        }
    | Success SuccessModel


type alias SuccessModel =
    { slug : String
    , title : String
    , date : Maybe Date
    , category : List String
    , raw : String
    , content : List (Html Msg)
    }


init : String -> ( Model, Cmd Msg )
init slug =
    ( Loading slug
    , Http.get
        { url =
            "https://raw.githubusercontent.com/kimkanu/kanu.kim/gh-pages/posts/"
                ++ slug
                ++ ".md"
        , expect = Http.expectString FetchPost
        }
    )



-- UPDATE


type Msg
    = FetchPost (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchPost response ->
            case response of
                Err _ ->
                    ( LoadingFailure (getSlug model), Cmd.none )

                Ok markdownWithYaml ->
                    let
                        content =
                            separateYaml markdownWithYaml

                        yamlDecoder =
                            Yaml.Decode.map4 YamlData
                                (Yaml.Decode.field "title" Yaml.Decode.string)
                                (Yaml.Decode.field "date" Yaml.Decode.string)
                                (Yaml.Decode.field "category" <|
                                    Yaml.Decode.list <|
                                        Yaml.Decode.map String.trim Yaml.Decode.string
                                )
                                (Yaml.Decode.sometimes <| Yaml.Decode.field "image" Yaml.Decode.string)

                        decodedYaml =
                            content.yaml
                                |> Maybe.map (Yaml.Decode.fromString yamlDecoder)

                        decodedMarkdown =
                            content.markdown
                                |> Markdown.toHtml Nothing
                                |> List.map Html.Styled.fromUnstyled
                    in
                    case decodedYaml of
                        Nothing ->
                            ( YamlFailure
                                { slug = getSlug model
                                , raw = markdownWithYaml
                                , error = "Missing yaml metadata."
                                }
                            , Cmd.none
                            )

                        Just justDecodedYaml ->
                            case justDecodedYaml of
                                Ok yaml ->
                                    ( Success
                                        { slug = getSlug model
                                        , title = yaml.title
                                        , date = Date.fromIsoString yaml.date |> Result.toMaybe
                                        , category = yaml.category
                                        , raw = markdownWithYaml
                                        , content = decodedMarkdown
                                        }
                                    , highlightSyntax ()
                                    )

                                Err x ->
                                    ( YamlFailure
                                        { slug = getSlug model
                                        , raw = markdownWithYaml
                                        , error =
                                            case x of
                                                Yaml.Decode.Parsing e ->
                                                    "Parsing error: " ++ e

                                                Yaml.Decode.Decoding e ->
                                                    "Decoding error: " ++ e
                                        }
                                    , Cmd.none
                                    )


type alias MarkdownWithYaml =
    { yaml : Maybe String
    , markdown : String
    }


type alias YamlData =
    { title : String
    , date : String
    , category : List String
    , image : Maybe String
    }


separateYaml : String -> MarkdownWithYaml
separateYaml markdownWithYaml =
    let
        splitted =
            String.split "---" markdownWithYaml
    in
    case splitted of
        [] ->
            MarkdownWithYaml Nothing markdownWithYaml

        [ _ ] ->
            MarkdownWithYaml Nothing markdownWithYaml

        _ :: yaml :: rest ->
            MarkdownWithYaml (Just yaml) (String.join "---" rest)


getSlug : Model -> String
getSlug model =
    case model of
        Loading slug ->
            slug

        LoadingFailure slug ->
            slug

        YamlFailure m ->
            m.slug

        Success m ->
            m.slug



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> { title : String, page : List (Html Msg) }
view model =
    case model of
        Success m ->
            { title = decapitalize m.title ++ " - kanu.kim"
            , page =
                [ main_ [ css [ height (pct 100), fontSize (em 1.16), color colorTheme.text ] ] <|
                    viewHeader
                        ++ viewContent m
                ]
            }

        YamlFailure m ->
            { title = m.slug ++ " - kanu.kim"
            , page =
                [ main_ [ css [ height (pct 100), fontSize (em 1.16), color colorTheme.text ] ] <|
                    viewHeader
                        ++ viewYamlError m.error
                ]
            }

        Loading slug ->
            { title = "kanu.kim"
            , page =
                [ main_ [ css [ height (pct 100), fontSize (em 1.16), color colorTheme.text ] ] <|
                    viewLoading slug
                ]
            }

        LoadingFailure slug ->
            { title = "Not found - kanu.kim"
            , page =
                [ main_ [ css [ height (pct 100), fontSize (em 1.16), color colorTheme.text ] ] <|
                    viewHeader
                        ++ viewNotFoundError slug
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


viewContent : SuccessModel -> List (Html Msg)
viewContent m =
    [ div [ css [ displayFlex, justifyContent center, alignItems center, marginBottom (px 72) ] ]
        [ div [ css [ width (pct 100), maxWidth (px 720), padding2 (px 0) (px 32), Css.boxSizing Css.borderBox ] ] <|
            [ h1
                [ css
                    [ fontFamilies [ "Regattia" ]
                    , fontWeight (int 900)
                    , fontSize (em 2.7)
                    , padding2 (px 0) (px 48)
                    , textAlign center
                    , lineHeight (em 1.14)
                    ]
                ]
                [ text m.title ]
            , div
                [ css
                    [ fontSize (em 0.85)
                    , color colorTheme.ambientText
                    , textAlign center
                    , padding3 (px 0) (px 48) (px 48)
                    , withMedia
                        [ only screen [ Media.maxWidth (px 540) ] ]
                        [ padding3 (px 0) (px 24) (px 48) ]
                    , fontFamilies [ "Mosk" ]
                    ]
                ]
              <|
                [ icon [] calendar
                , text <| " " ++ maybeDateToString m.date
                , span [ css [ padding2 (px 0) (px 4) ] ] [ text " | " ]
                ]
                    ++ viewCategory m.category
            , div [ css [ fontFamilies [ "Mosk" ] ] ] m.content
            ]
        ]
    ]


viewLoading : String -> List (Html Msg)
viewLoading m =
    [ div
        [ css [ height (pct 100), displayFlex, flexDirection column, justifyContent center, alignItems center ] ]
        [ div [ css [ width (px 112) ] ]
            [ img [ src smallLogoImage, css [ width (px 112) ] ] [] ]
        , div [ css [ textAlign center, fontFamilies [ "Mosk" ] ] ]
            [ span [] [ text "Loading " ]
            , span [ css [ fontWeight bold ] ] [ m |> Url.percentDecode |> Maybe.withDefault "" |> text ]
            , span [] [ text "..." ]
            ]
        ]
    ]


viewNotFoundError : String -> List (Html Msg)
viewNotFoundError slug =
    [ div [ css [ displayFlex, justifyContent center, alignItems center, height (calc (vh 100) minus (px 80)) ] ]
        [ div [ css [ width (pct 100), maxWidth (px 400), Css.boxSizing Css.borderBox, textAlign center ] ] <|
            [ h1
                [ css
                    [ fontFamilies [ "Regattia" ]
                    , fontWeight (int 900)
                    , fontSize (em 8)
                    , textAlign center
                    , lineHeight (em 1)
                    , Css.margin3 (px 0) (px 0) (px 32)
                    , color colorTheme.lightGray
                    ]
                ]
                [ text "∄" ]
            , p []
                [ span [ css [ fontFamilies [ "Mosk" ], color colorTheme.text ] ]
                    [ text <| "There is no page at the destination /blog/" ++ slug ++ "."
                    ]
                ]
            , p []
                [ a [ href "/blog", css [ Css.textDecoration Css.none, color colorTheme.ambientText ] ]
                    [ icon
                        [ display Css.inlineBlock, fontFamilies [ "BoxIcons" ], color colorTheme.text, Css.transform <| Css.translateY (px 3), color colorTheme.ambientText ]
                        backArrow
                    , span [ css [ fontFamilies [ "Mosk" ] ] ]
                        [ text <| " Return to the list"
                        ]
                    ]
                ]
            ]
        ]
    ]


viewYamlError : String -> List (Html Msg)
viewYamlError error =
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
                [ icon [] caution ]
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


port highlightSyntax : () -> Cmd msg
