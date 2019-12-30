module Main exposing (main)

import Browser
import Browser.Dom
import Browser.Navigation as Nav
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css, src)
import Page.Home
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
    | Loading
    | Error String


init : flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    onNavigation
        { route = Route.fromUrl url
        , page = Loading
        , key = key
        }



-- UPDATE


type Msg
    = NoOp
    | OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | PageMsg PageMsg


type PageMsg
    = HomeMsg Page.Home.Msg
    | ShortenerMsg Page.Shortener.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnUrlChange url ->
            onNavigation { model | route = Route.fromUrl url }

        OnUrlRequest (Browser.Internal url) ->
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

                _ ->
                    ( model, Cmd.none )


mapPage : Model -> (a -> Page) -> (msg -> PageMsg) -> ( a, Cmd msg ) -> ( Model, Cmd Msg )
mapPage model toPage toMsg ( page, cmds ) =
    ( { model | page = toPage page }
    , Cmd.map (PageMsg << toMsg) cmds
    )


{-| Map a Route (a parsed url) to a Page and initialize the modules Model.
This is the place you fetch data to render the page.
-}
onNavigation : Model -> ( Model, Cmd Msg )
onNavigation model =
    scrollOnNav model.page <|
        case model.route of
            Route.Root ->
                mapPage model Home HomeMsg <|
                    Page.Home.init

            Route.Shortener ->
                mapPage model Shortener ShortenerMsg <|
                    Page.Shortener.init

            Route.NotFound ->
                ( model, Cmd.none )


{-| Scroll to top of page on navigation.
Do this if you navigate without the need to reset the scroll position.
case ( currentPage, model.route ) of
( SomePage model\_, SomePageRoute arguments ) ->
( model, cmds )
\_ ->
( model
, Cmd.batch
[ cmds
, Task.perform (always NoOp) (Browser.Dom.setViewport 0 0)
]
)
-}
scrollOnNav : Page -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
scrollOnNav currentPage ( model, cmds ) =
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
        shortnerSubscriptions =
            case model.page of
                Shortener shortenerModel ->
                    Page.Shortener.subscriptions shortenerModel

                _ ->
                    Sub.none
    in
    Platform.Sub.batch
        [ Platform.Sub.map (ShortenerMsg >> PageMsg) shortnerSubscriptions
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

        Loading ->
            { title = "", page = [] }

        Error err ->
            { title = "", page = [ text err ] }
