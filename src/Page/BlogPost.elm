port module Page.BlogPost exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Css exposing (alignItems, backgroundColor, block, bold, border, borderRadius, boxShadow5, calc, center, color, column, cursor, default, display, displayFlex, em, flexDirection, fontFamilies, fontFamily, fontSize, fontWeight, height, hex, hover, int, justifyContent, lineHeight, marginBottom, marginLeft, marginRight, maxHeight, maxWidth, minus, none, outline, padding2, padding3, pct, pointer, px, textAlign, vw, wait, width)
import Date exposing (Date)
import Debug exposing (log)
import Html.Styled exposing (Attribute, Html, b, button, div, h1, img, input, main_, span, text)
import Html.Styled.Attributes as Attr exposing (css, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Markdown
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
        { url = "/posts/" ++ slug ++ ".md"
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
                                (Yaml.Decode.field "category" (Yaml.Decode.list Yaml.Decode.string))
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
            { title = m.title ++ " - kanu.kim"
            , page =
                [ main_ [ css [ height (pct 100), fontSize (em 1.16), color colorTheme.text ] ] <|
                    viewHeader
                        ++ viewContent m
                ]
            }

        YamlFailure m ->
            { title = " - kanu.kim"
            , page =
                [ main_ [ css [ height (pct 100), fontSize (em 1.16), color colorTheme.text ] ]
                    [ text m.error ]
                ]
            }

        Loading m ->
            { title = "kanu.kim"
            , page =
                [ main_ [ css [ height (pct 100), fontSize (em 1.16), color colorTheme.text ] ] <|
                    viewLoading m
                ]
            }

        _ ->
            { title = "hh"
            , page =
                [ main_ []
                    [ text "ss"
                    ]
                ]
            }


viewHeader : List (Html Msg)
viewHeader =
    [ div [ css [ height (px 80), displayFlex, justifyContent center, alignItems center ] ]
        [ div [ css [ width (px 56) ] ]
            [ img [ src smallLogoImage, css [ width (px 56) ] ] [] ]
        , div [ css [ textAlign center, fontSize (em 1.5), marginLeft (px 24) ] ]
            [ span [ css [ fontWeight (int 700), fontFamilies [ "Metropolis" ], color colorTheme.boldText ] ] [ text "blog://" ]
            , span [ css [ fontFamilies [ "Mosk" ], marginLeft (px 2) ] ] [ text "kanu.kim" ]
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
                    ]
                ]
                [ text <| (Maybe.withDefault "Someday" <| Maybe.map (Date.format "MMM ddd, y") m.date) ++ " | " ++ String.join " · " m.category ]
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
            , span [ css [ fontWeight bold ] ] [ text m ]
            , span [] [ text "..." ]
            ]
        ]
    ]


type alias ColorTheme =
    { boldText : Css.Color
    , text : Css.Color
    , ambientText : Css.Color
    , red : Css.Color
    }


colorTheme : ColorTheme
colorTheme =
    { boldText = hex "#424242"
    , text = hex "#3a3a3c"
    , ambientText = hex "#657786"
    , red = hex "#E45B79"
    }


smallLogoImage : String
smallLogoImage =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAHAAAABwCAIAAABJgmMcAAAABGdBTUEAALGPC/xhBQAAACBjSFJNAAB6JgAAgIQAAPoAAACA6AAAdTAAAOpgAAA6mAAAF3CculE8AAAABmJLR0QA/wD/AP+gvaeTAAAAB3RJTUUH5AISEycrqchpTAAABMNJREFUeNrtnc1u20gMx4cjR0aSyxZogRaV7XbdrZ3H2ktfOIFju03iuH6A7iWArA/2IFirtk4ysilyIvF/DJBQ8xuSM8PhIICIRkUnK/0BbZMCJZYCJZYCJZYCJZYCJZYCJZYCJZYCJZYCJZYCJVYNoHrod1EtD1Wiz8sVaBzHt7crLU09K1eg1gar1ffZbGmMUaxPyD3kMQzD9fr79fUSAJTpY6q1KGEYhvf3G2X6hOptmxCx3z+5v1c/fVS196F5XvipMt2vQzb2u9hXpnt04ElJmT6mw4+eynSvjjrLK9M/dWxxRJn+JoJqkzKtiqZ8p0xLkdVDS6bzeaeZUhaYC6arVaeZElfsS6adjX36K5CO59NG7pS6zLSpS7rOMm3w1rOb9dNmr5ERMQy7VT9t/F6+a7HP0ejQKaZMnSNlPp3Pv7abKV8rTpFPV6t1u/2UtbepC7HP3SzWeqYC3XftZirTzthipmL9oW1lKtlw20qmwh3M7WMq3xLeMqbyQE27mHoB1LSIqS9ATVuYegTUtIKpX0DNy2fqHVDzwpn2ao7UAHB8VsnUGHNx8VmcKTgPuwZQAGstuP/p49Xv99frDQBMp/+wGT1S7kBhu42TJLXWcvoLAHz7dpum6adPHxGRczqrX9Hvh46mXYFaa3/8+O/y8rLXq5UlCISIV1dXHz6M7u7uACwA36tTAMiy7Pz8/MuXf09PT11m1JVOlmXv3r19eHhYLBZBEDANyBize7j3+vWbNM1msxnvjEKWZdaC+xS6fhyASZJ0OBwGgb2+ngdBwLP+AkCe54WnjEYjAKjMKBrDkAHQ2hoOVGtRgjiOo2iQ58jmp9U52263URQh4q9MG9QuwGtYqbcPBYDtdjscDieTSZqmjQ5mr/UkSQrrWZYxW3dU7Y19wXQwGFxcTPlHVVqfTj1leshJqfCUwWA4ncowTZIkijz10wOPnhVPkWK69TP2Dz/LV6NPJJ8W2VxkRp/QUcWRYlRRJOanxbrvlZ8eW22SXXk9XPcJyney0Vex7gVTmnqo+BolmHl+E1mBWZxpkiSDwUA89ikr9iXTyeSzYOzLMiW+AtmNaiSdT8Vin/5OSXY3U1qXWqMauaQrdzOi+VQm9pu69ZStYgjm0wavkXdMJfen/LuOZu/lxfMp/16q8UYH8bMpc+xzdI6I7/k5Mw9TK04ZfVJrVJIwZR6+3ibxNYon87A2i1XOppL5tNEZ5e6+8yGfRlHUnHWBdsbKbkamhtLoXkqmP9STGkoTTMUabn2I/SbyqWQHsw9MyfOpcEu4+LpPnk/le+zF+6Vo86k8UNOufikvgJoW9Uv5AtR4sEaR9Et5BNTs65fifKVAspfyC6j5o2uB+YXS8RVx7icdjqMqKkPGmMViYS13Bae0vlwui5+5//ohHcw8oyqir1z3RWJ/56fobryGhyIiIuZ5zjawOI7fv4+yLL+5ucnzvPgATutRFKVputls3O3WeEkXhieG11OMMYg4Hv99dnZ2ctILwxAxZ3lK87/98Xj86tVfO57Pm67x1ihJUpH/84dogiDI81zKeq8XuOfxF/Z42n95t2166VKgxFKgxFKgxFKgxFKgxFKgxFKgxFKgxFKgxFKgxFKgxPoJSmBXrfevUPEAAAAldEVYdGRhdGU6Y3JlYXRlADIwMjAtMDItMThUMTk6Mzk6NDMrMDk6MDA7Wsm3AAAAJXRFWHRkYXRlOm1vZGlmeQAyMDIwLTAyLTE4VDE5OjM5OjQzKzA5OjAwSgdxCwAAAABJRU5ErkJggg=="


port highlightSyntax : () -> Cmd msg
