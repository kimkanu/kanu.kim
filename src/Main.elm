module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Navigation as Nav
import Html.Styled exposing (Html, text, toUnstyled)
import Page.BlogList
import Page.BlogPost exposing (Model(..))
import Page.Home
import Page.ScrollTo exposing (scrollTo)
import Page.Shortener
import Platform.Sub
import Route exposing (Route)
import Task
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }



-- MODEL


type alias Model =
    { route : Route
    , page : Page
    , key : Nav.Key
    }


type Page
    = Home Page.Home.Model
    | Shortener Page.Shortener.Model
    | BlogList Page.BlogList.Model
    | BlogPost Page.BlogPost.Model
    | Loading
    | Error ErrorModel


type alias ErrorModel =
    { code : Int
    , message : String
    }


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    onNavigation
        { route = Route.fromUrl url
        , page = Loading
        , key = key
        }
        Nothing



-- UPDATE


type Msg
    = NoOp
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | PageMsg PageMsg


type PageMsg
    = HomeMsg Page.Home.Msg
    | ShortenerMsg Page.Shortener.Msg
    | BlogListMsg Page.BlogList.Msg
    | BlogPostMsg Page.BlogPost.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnUrlChange url ->
            onNavigation { model | route = Route.fromUrl url } (Just model.route)

        OnUrlRequest (Browser.Internal url) ->
            {-

               case model.route of
                   Route.BlogPost slug _ ->
                       case Route.fromUrl url of
                           Route.BlogPost slug_ fragment ->
                               if slug == slug_ then
                                   ( model, pushFragment <| Maybe.withDefault "" fragment )
                               else
                                   ( model, Nav.pushUrl model.key (Url.toString url) )

                           _ ->
                               ( model, Nav.pushUrl model.key (Url.toString url) )


                   _ ->
                       ( model, Nav.pushUrl model.key (Url.toString url) )

            -}
            ( model, Nav.pushUrl model.key (Url.toString url) )

        OnUrlRequest (Browser.External href) ->
            ( model, Nav.load href )

        PageMsg pageMsg ->
            case ( model.page, pageMsg ) of
                ( Home model_, HomeMsg homeMsg ) ->
                    mapPage model Home HomeMsg <|
                        Page.Home.update homeMsg model_

                ( Shortener model_, ShortenerMsg shortenerMsg ) ->
                    mapPage model Shortener ShortenerMsg <|
                        Page.Shortener.update shortenerMsg model_

                ( BlogList model_, BlogListMsg blogListMsg ) ->
                    mapPage model BlogList BlogListMsg <|
                        Page.BlogList.update blogListMsg model_

                ( BlogPost model_, BlogPostMsg blogPostMsg ) ->
                    mapPage model BlogPost BlogPostMsg <|
                        Page.BlogPost.update blogPostMsg model_

                _ ->
                    ( { model | page = Error <| ErrorModel 404 "Not found." }, Cmd.none )


mapPage : Model -> (a -> Page) -> (msg -> PageMsg) -> ( a, Cmd msg ) -> ( Model, Cmd Msg )
mapPage model toPage toMsg ( page, cmds ) =
    ( { model | page = toPage page }
    , Cmd.map (PageMsg << toMsg) cmds
    )


{-| Map a Route (a parsed url) to a Page and initialize the modules Model.
This is the place you fetch data to render the page.
-}
onNavigation : Model -> Maybe Route -> ( Model, Cmd Msg )
onNavigation model prevRoute =
    let
        scrollOrNot =
            case ( model.route, prevRoute ) of
                ( Route.BlogPost slug _, Just (Route.BlogPost slug_ _) ) ->
                    if slug == slug_ then
                        identity

                    else
                        scrollToTop

                _ ->
                    scrollToTop
    in
    scrollOrNot <|
        case model.route of
            Route.Root ->
                mapPage model Home HomeMsg <|
                    Page.Home.init

            Route.Shortener ->
                mapPage model Shortener ShortenerMsg <|
                    Page.Shortener.init

            Route.BlogList maybeQuery ->
                mapPage model BlogList BlogListMsg <|
                    Page.BlogList.init maybeQuery

            Route.BlogPost slug frag ->
                case ( prevRoute, model.page ) of
                    ( Just (Route.BlogPost slug_ _), BlogPost m ) ->
                        if slug == slug_ then
                            mapPage model BlogPost BlogPostMsg ( Page.BlogPost.updateFragment frag m, scrollTo <| Maybe.withDefault "" frag )

                        else
                            mapPage model BlogPost BlogPostMsg <|
                                Page.BlogPost.init slug frag

                    _ ->
                        mapPage model BlogPost BlogPostMsg <|
                            Page.BlogPost.init slug frag

            Route.NotFound ->
                ( { model | page = Error <| ErrorModel 404 "Not found." }, Cmd.none )


scrollToTop : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
scrollToTop ( model, cmds ) =
    ( model
    , Cmd.batch
        [ cmds
        , Task.perform (always NoOp) (Browser.Dom.setViewport 0 0)
        ]
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subs =
            case model.page of
                Shortener shortenerModel ->
                    Sub.map ShortenerMsg <| Page.Shortener.subscriptions shortenerModel

                _ ->
                    Sub.none
    in
    Platform.Sub.batch
        [ Platform.Sub.map PageMsg subs
        ]



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        { title, page } =
            viewPage model
    in
    { title = title
    , body =
        List.concat
            [ page
            ]
            |> List.map toUnstyled
    }


viewPage : Model -> { title : String, page : List (Html Msg) }
viewPage model =
    let
        map pageMsg { title, page } =
            { title = title
            , page = List.map (Html.Styled.map pageMsg) page
            }
    in
    case model.page of
        Home model_ ->
            map (PageMsg << HomeMsg) (Page.Home.view model_)

        Shortener model_ ->
            map (PageMsg << ShortenerMsg) (Page.Shortener.view model_)

        BlogList model_ ->
            map (PageMsg << BlogListMsg) (Page.BlogList.view model_)

        BlogPost model_ ->
            map (PageMsg << BlogPostMsg) (Page.BlogPost.view model_)

        Loading ->
            { title = "", page = [ text "" ] }

        Error err ->
            viewError err


viewError : ErrorModel -> { title : String, page : List (Html Msg) }
viewError error =
    case error.code of
        404 ->
            { title = "404 Not found - kanu.kim"
            , page = [ text "404" ]
            }

        _ ->
            { title = "Unknown error occurred - kanu.kim"
            , page = [ text "Unknown error occurred." ]
            }
