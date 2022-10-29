module Main exposing (main)

import Browser
import Browser.Events
import Dict
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Region as Region
import GameBoard exposing (GameBoard, Shape(..))
import Html.Attributes
import Random
import TileSvg


defaultBoardSize : Int
defaultBoardSize =
    4


minBoardSize : Int
minBoardSize =
    3


maxBoardSize : Int
maxBoardSize =
    20


darkBlue : Color
darkBlue =
    rgb255 17 43 60


lightBlue : Color
lightBlue =
    rgb255 32 83 117


white : Color
white =
    rgb 1 1 1


grey : Color
grey =
    rgb255 150 150 150


headerHeight : Int
headerHeight =
    80


minWindowSize : Int
minWindowSize =
    300


minTileWidth : Int
minTileWidth =
    140


getTileSize : Int -> Int -> Int
getTileSize windowSize boardSize =
    let
        gameWindowWidth =
            (toFloat (max minWindowSize windowSize) * 0.7) |> round |> roundToEven

        widthPerTile =
            gameWindowWidth // boardSize |> roundToEven
    in
    min widthPerTile minTileWidth


type alias Flags =
    { number : Int, windowWidth : Int, windowHeight : Int }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- Model


type alias Model =
    { boardSize : Int
    , board : GameBoard
    , seed : Random.Seed
    , solved : Bool
    , windowSize : Int
    , tileSize : Int
    , history : List GameBoard
    }



-- Int


init : Flags -> ( Model, Cmd Msg )
init { number, windowWidth, windowHeight } =
    let
        ( board, seed ) =
            GameBoard.initGameBoard defaultBoardSize (Random.initialSeed number)

        windowSize =
            min windowWidth windowHeight
    in
    ( { board = board
      , boardSize = defaultBoardSize
      , history = []
      , seed = seed
      , solved = False
      , tileSize = getTileSize windowSize defaultBoardSize
      , windowSize = windowSize
      }
    , Cmd.none
    )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\width height -> WindowResized width height)



-- Update


type Msg
    = NewSeedRequested
    | NewSeedReceived Int
    | BoardSizeDecreased
    | BoardSizeIncreased
    | TileRotated GameBoard.Coords
    | UndoMove
    | WindowResized Int Int


generateSeed : Cmd Msg
generateSeed =
    Random.generate NewSeedReceived (Random.int Random.minInt Random.maxInt)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewSeedRequested ->
            ( model, generateSeed )

        NewSeedReceived int ->
            handleNewSeedReceived int model

        BoardSizeDecreased ->
            handleBoardSizeChange (max minBoardSize (model.boardSize - 1)) model

        BoardSizeIncreased ->
            handleBoardSizeChange (min (model.boardSize + 1) maxBoardSize) model

        TileRotated coords ->
            handleTileRotated coords model

        UndoMove ->
            handleUndoMove model

        WindowResized width height ->
            handleWindowResized width height model


handleNewSeedReceived : Int -> Model -> ( Model, Cmd Msg )
handleNewSeedReceived int model =
    let
        ( newBoard, newSeed ) =
            GameBoard.initGameBoard model.boardSize (Random.initialSeed int)
    in
    ( { model | board = newBoard, history = [], seed = newSeed, solved = False }, Cmd.none )


handleTileRotated : GameBoard.Coords -> Model -> ( Model, Cmd Msg )
handleTileRotated coords ({ board, solved, history } as model) =
    let
        newBoard =
            GameBoard.rotateCell coords board

        isSolved =
            solved
                || List.all
                    (\x -> x == 0)
                    (List.map (\{ rotations } -> rotations) (Dict.values newBoard))
    in
    ( { model | board = newBoard, solved = isSolved, history = board :: history }, Cmd.none )


handleUndoMove : Model -> ( Model, Cmd Msg )
handleUndoMove ({ history } as model) =
    case history of
        [] ->
            ( model, Cmd.none )

        lastBoard :: historyTail ->
            ( { model | board = lastBoard, history = historyTail }, Cmd.none )


handleBoardSizeChange : Int -> Model -> ( Model, Cmd Msg )
handleBoardSizeChange newBoardSize model =
    let
        ( newBoard, newSeed ) =
            GameBoard.initGameBoard newBoardSize model.seed
    in
    ( { boardSize = newBoardSize
      , board = newBoard
      , seed = newSeed
      , solved = False
      , windowSize = model.windowSize
      , tileSize = getTileSize model.windowSize newBoardSize
      , history = []
      }
    , Cmd.none
    )


handleWindowResized : Int -> Int -> Model -> ( Model, Cmd Msg )
handleWindowResized width height model =
    let
        windowSize =
            min width (height - headerHeight)
    in
    ( { model | windowSize = windowSize, tileSize = getTileSize windowSize model.boardSize }, Cmd.none )



-- View


view : Model -> Browser.Document Msg
view ({ board, boardSize, tileSize } as model) =
    let
        tiles =
            Dict.foldr (\k v acc -> viewCell tileSize k v :: acc) [] board
    in
    { title = "Graph Bang"
    , body =
        [ layout []
            (viewBody
                [ viewHeader model
                , viewGameWindow boardSize tiles
                ]
            )
        ]
    }


viewBody : List (Element msg) -> Element msg
viewBody contents =
    column
        [ width fill, height fill, Background.color darkBlue, Font.color white ]
        contents


viewGameWindow : Int -> List (Element Msg) -> Element Msg
viewGameWindow boardSize tiles =
    let
        rows =
            split boardSize tiles

        renderedRows =
            List.map (\x -> viewBoardRow x) rows
    in
    column
        [ centerX
        , padding 40
        , Border.width TileSvg.borderWidth
        , Border.color darkBlue
        ]
        renderedRows


viewHeader : Model -> Element Msg
viewHeader model =
    row [ width fill, height (px headerHeight), padding 60 ]
        [ row [ width (fillPortion 1), spacing 4 ]
            [ viewNewButton
            , viewDecBoardSizeButton model
            , viewIncBoardSizeButton model
            , viewUndoButton model
            ]
        , viewWinMessage model
        , el [ width (fillPortion 1) ] Element.none
        ]


viewNewButton : Element Msg
viewNewButton =
    Input.button
        [ width shrink
        , height (px 60)
        , Font.center
        , padding 10
        , Border.rounded 6
        , Background.color lightBlue
        , htmlAttribute (Html.Attributes.style "touch-action" "manipulation")
        ]
        { onPress = Just NewSeedRequested, label = text "New" }


viewDecBoardSizeButton : Model -> Element Msg
viewDecBoardSizeButton model =
    if model.boardSize <= minBoardSize then
        viewBoardSizeButton grey { onPress = Nothing, label = text "-" }

    else
        viewBoardSizeButton lightBlue { onPress = Just BoardSizeDecreased, label = text "-" }


viewIncBoardSizeButton : Model -> Element Msg
viewIncBoardSizeButton model =
    if model.boardSize >= maxBoardSize then
        viewBoardSizeButton grey { onPress = Nothing, label = text "+" }

    else
        viewBoardSizeButton lightBlue { onPress = Just BoardSizeIncreased, label = text "+" }


viewBoardSizeButton : Color -> { onPress : Maybe Msg, label : Element Msg } -> Element Msg
viewBoardSizeButton color options =
    Input.button
        [ width (px 60)
        , height (px 60)
        , Font.center
        , padding 10
        , Border.rounded 6
        , Background.color color
        , htmlAttribute (Html.Attributes.style "touch-action" "manipulation")
        , Region.description "board at min size"
        ]
        options


viewUndoButton : Model -> Element Msg
viewUndoButton model =
    if List.length model.history == 0 then
        Element.none

    else
        Input.button
            [ width shrink
            , height (px 60)
            , Font.center
            , padding 10
            , Border.rounded 6
            , Background.color lightBlue
            , htmlAttribute (Html.Attributes.style "touch-action" "manipulation")
            ]
            { onPress = Just UndoMove, label = text "Undo" }


viewWinMessage : Model -> Element Msg
viewWinMessage model =
    el [ width (fillPortion 1), Font.center ]
        (if model.solved then
            row [ centerX ]
                [ el [ padding 40, Font.center, Font.size 40 ] (text "🎉 🥳 🎉")
                , Input.button
                    [ width (px 140)
                    , height (px 60)
                    , centerX
                    , Font.center
                    , Border.rounded 6
                    , Background.color lightBlue
                    ]
                    { onPress = Just NewSeedRequested, label = text "Play Again" }
                ]

         else
            Element.none
        )


viewBoardRow : List (Element msg) -> Element msg
viewBoardRow elements =
    row [ centerX, centerY ] elements


viewCell : Int -> GameBoard.Coords -> GameBoard.Cell -> Element Msg
viewCell tileSize coords cell =
    el
        [ centerX
        , centerY
        , Border.width TileSvg.borderWidth
        , Border.color darkBlue
        , Border.rounded
            (if tileSize < 80 then
                2

             else
                6
            )
        , Background.color lightBlue
        , rotate (degrees (toFloat ((cell.initRotations + cell.rotations) * 90)))
        , htmlAttribute (Html.Attributes.style "touch-action" "manipulation")
        , Events.onClick (TileRotated coords)
        ]
        (case cell.shape of
            Knob ->
                TileSvg.knobSvg tileSize

            Bar ->
                TileSvg.barSvg tileSize

            Elbow ->
                TileSvg.elbowSvg tileSize

            Tee ->
                TileSvg.teeSvg tileSize

            Empty ->
                Element.none
        )



-- Helpers


roundToEven : Int -> Int
roundToEven int =
    2 * (int // 2)


split : Int -> List a -> List (List a)
split toTake list =
    case list of
        [] ->
            []

        _ ->
            List.take toTake list :: split toTake (List.drop toTake list)
