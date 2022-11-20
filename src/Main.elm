module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Page.Colors exposing (darkBlue, white)
import Page.Graph as Graph
import Page.Snake as Snake
import Route exposing (Route)
import Url exposing (Url)



-- MAIN


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { route : Route
    , page : Page
    , navKey : Nav.Key
    , intTime : Int
    , windowWidth : Int
    , windowHeight : Int
    }


type Page
    = NotFoundPage
    | HomePage
    | GraphPage Graph.Model
    | SnakePage Snake.Model


type Msg
    = LinkClicked UrlRequest
    | UrlChanged Url
    | GraphMsg Graph.Msg
    | SnakeMsg Snake.Msg


type alias Flags =
    { intTime : Int, windowWidth : Int, windowHeight : Int }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        aurl =
            Debug.log (Url.toString url)

        model =
            { route = Route.parseUrl url
            , page = NotFoundPage
            , navKey = navKey
            , intTime = flags.intTime
            , windowWidth = flags.windowWidth
            , windowHeight = flags.windowHeight
            }
    in
    initCurrentPage ( model, Cmd.none )


initCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
initCurrentPage ( model, existingCmds ) =
    let
        ( currentPage, mappedPageCmds ) =
            case model.route of
                Route.NotFound ->
                    ( NotFoundPage, Cmd.none )

                Route.HomeRoute ->
                    ( HomePage, Cmd.none )

                Route.GraphRoute ->
                    let
                        ( pageModel, pageCmds ) =
                            Graph.init
                                { number = model.intTime
                                , windowWidth = model.windowWidth
                                , windowHeight = model.windowHeight
                                }
                    in
                    ( GraphPage pageModel, Cmd.map GraphMsg pageCmds )

                Route.SnakeRoute ->
                    let
                        ( pageModel, pageCmds ) =
                            Snake.init
                                { number = model.intTime
                                , windowWidth = model.windowWidth
                                , windowHeight = model.windowHeight
                                }
                    in
                    ( SnakePage pageModel, Cmd.map SnakeMsg pageCmds )
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


view : Model -> Document Msg
view model =
    { title = "lowderdev"
    , body =
        [ layout
            [ Background.color darkBlue
            , Font.color white
            ]
            (currentView model)
        ]
    }


currentView : Model -> Element Msg
currentView model =
    case model.page of
        NotFoundPage ->
            notFoundView

        HomePage ->
            homeView

        GraphPage pageModel ->
            Graph.view pageModel
                |> Element.map GraphMsg

        SnakePage pageModel ->
            Snake.view pageModel
                |> Element.map SnakeMsg


notFoundView : Element msg
notFoundView =
    row [ centerX, padding 100 ]
        [ el [] (text "Oops! The page you requested was not found!")
        ]


homeView : Element Msg
homeView =
    column [ centerX, padding 100 ]
        [ el [] (text "Hi. My name is Logan and this is a website.")
        , el [] (text "Here are some things I've made:")
        , link [ paddingXY 20 0 ] { url = "/graph", label = el [ Font.underline ] (text "Graph") }
        , link [ paddingXY 20 0 ] { url = "/snake", label = el [ Font.underline ] (text "Snake") }
        ]



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( LinkClicked urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        ( UrlChanged url, _ ) ->
            let
                newRoute =
                    Route.parseUrl url
            in
            ( { model | route = newRoute }, Cmd.none )
                |> initCurrentPage

        ( GraphMsg subMsg, GraphPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    Graph.update subMsg pageModel
            in
            ( { model | page = GraphPage updatedPageModel }
            , Cmd.map GraphMsg updatedCmd
            )

        ( SnakeMsg subMsg, SnakePage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    Snake.update subMsg pageModel
            in
            ( { model | page = SnakePage updatedPageModel }
            , Cmd.map SnakeMsg updatedCmd
            )

        ( _, _ ) ->
            ( model, Cmd.none )
