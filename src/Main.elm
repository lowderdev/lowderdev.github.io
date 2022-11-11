module Main exposing (Model, Msg(..), init, main, subscriptions, update, view, viewLink)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Page.Graph
import Url
import Url.Parser exposing (oneOf, top)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    }


type Page
    = Home
    | Graph


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model key url, Cmd.none )



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        parser =
            oneOf
                [ Url.Parser.map Home top
                , Url.Parser.map Graph (Url.Parser.s "graph")
                ]
    in
    case Url.Parser.parse parser model.url of
        Just Home ->
            { title = "URL Interceptor"
            , body =
                [ text "The current URL is: "
                , b [] [ text (Url.toString model.url) ]
                , ul []
                    [ viewLink "/home"
                    , viewLink "/graph"
                    , viewLink "/profile"
                    , viewLink "/reviews/the-century-of-the-self"
                    , viewLink "/reviews/public-opinion"
                    , viewLink "/reviews/shah-of-shahs"
                    ]
                ]
            }

        Just Graph ->
            Page.Graph.view (Page.Graph.init ())

        _ ->
            { title = "URL Interceptor"
            , body = [ text "404" ]
            }


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]
