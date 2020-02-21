---
title: Hello World
date: 2020-02-21
category: [ second post, hello ]
---


Some `inline code`.

$formula$

$$
    formula
$$


$$
    formula
$$


$$
    formula
$$

```elm
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


```


mmymy