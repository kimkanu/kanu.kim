module Page.BlogPost exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , view
    )

import Css exposing (alignItems, backgroundColor, block, bold, border, borderRadius, boxShadow5, calc, center, color, column, cursor, default, display, displayFlex, em, flexDirection, fontFamilies, fontFamily, fontSize, fontWeight, height, hex, hover, int, justifyContent, lineHeight, marginBottom, marginLeft, marginRight, maxHeight, maxWidth, minus, none, outline, padding2, pct, pointer, px, textAlign, vw, wait, width)
import Date exposing (Date)
import Debug exposing (log)
import Html.Styled exposing (Attribute, Html, b, button, div, input, main_, span, text)
import Html.Styled.Attributes as Attr exposing (css, src, type_, value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Json.Decode as D
import Json.Encode as E
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
    | Success
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


sequenceResult : Result (List String) (Result String a) -> Result (List String) a
sequenceResult r =
    case r of
        Ok r1 ->
            r1
                |> Result.mapError (\e -> [ e ])

        Err x ->
            Err x


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
                            Yaml.Decode.map3 YamlData
                                (Yaml.Decode.field "title" Yaml.Decode.string)
                                (Yaml.Decode.field "date" Yaml.Decode.string)
                                (Yaml.Decode.field "category" (Yaml.Decode.list Yaml.Decode.string))

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
                                    , Cmd.none
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
                [ main_ []
                    m.content
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
