module Main exposing (main)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Element exposing (..)
import Element.Font as Font
import Html exposing (Html)
import Page.Graph as Graph
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


type Msg
    = GraphMsg Graph.Msg
    | LinkClicked UrlRequest
    | UrlChanged Url


type alias Flags =
    { intTime : Int, windowWidth : Int, windowHeight : Int }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
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
    in
    ( { model | page = currentPage }
    , Cmd.batch [ existingCmds, mappedPageCmds ]
    )


view : Model -> Document Msg
view model =
    { title = "lowderdev"
    , body =
        [ layout [] (currentView model) ]
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
        , link [ paddingXY 20 0 ] { url = "/graph", label = el [ Font.underline ] (text "Graph game") }
        ]



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.page ) of
        ( GraphMsg subMsg, GraphPage pageModel ) ->
            let
                ( updatedPageModel, updatedCmd ) =
                    Graph.update subMsg pageModel
            in
            ( { model | page = GraphPage updatedPageModel }
            , Cmd.map GraphMsg updatedCmd
            )

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

        ( _, _ ) ->
            ( model, Cmd.none )
