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
    12


darkBlue : Color
darkBlue =
    rgb255 17 43 60


lightBlue : Color
lightBlue =
    rgb255 32 83 117


white : Color
white =
    rgb 1 1 1


roundToEven : Int -> Int
roundToEven int =
    2 * (int // 2)


getTileSize : Int -> Int -> Int
getTileSize windowWidth boardSize =
    let
        gameWindowWidth =
            (toFloat windowWidth * 0.8) |> round |> roundToEven
    in
    gameWindowWidth // boardSize |> roundToEven


type alias Flags =
    { number : Int, windowWidth : Int }


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias Model =
    { boardSize : Int
    , board : GameBoard
    , seed : Random.Seed
    , solved : Bool
    , windowWidth : Int
    , tileSize : Int
    }


init : Flags -> ( Model, Cmd Msg )
init { number, windowWidth } =
    let
        ( board, seed ) =
            GameBoard.initGameBoard defaultBoardSize (Random.initialSeed number)
    in
    ( { boardSize = defaultBoardSize
      , board = board
      , seed = seed
      , solved = False
      , windowWidth = windowWidth
      , tileSize = getTileSize windowWidth defaultBoardSize
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onResize (\width _ -> GotResize width)


type Msg
    = GotResize Int
    | GenerateSeed
    | NewSeed Int
    | RotateTile GameBoard.Coords
    | DecSize
    | IncSize


generateSeed : Cmd Msg
generateSeed =
    Random.generate NewSeed (Random.int Random.minInt Random.maxInt)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResize windowWidth ->
            ( { model | windowWidth = windowWidth, tileSize = getTileSize windowWidth model.boardSize }, Cmd.none )

        GenerateSeed ->
            ( model, generateSeed )

        NewSeed int ->
            let
                ( newBoard, newSeed ) =
                    GameBoard.initGameBoard model.boardSize (Random.initialSeed int)
            in
            ( { model | board = newBoard, seed = newSeed, solved = False }, Cmd.none )

        RotateTile coords ->
            let
                newBoard =
                    GameBoard.rotateCell coords model.board

                isSolved =
                    List.all
                        (\x -> x == 0)
                        (List.map (\{ rotations } -> rotations) (Dict.values newBoard))
            in
            if model.solved then
                ( model, Cmd.none )

            else
                ( { model | board = newBoard, solved = isSolved }, Cmd.none )

        DecSize ->
            let
                newBoardSize =
                    max minBoardSize (model.boardSize - 1)

                ( newBoard, newSeed ) =
                    GameBoard.initGameBoard newBoardSize model.seed
            in
            ( { model
                | boardSize = newBoardSize
                , board = newBoard
                , seed = newSeed
                , solved = False
                , tileSize = getTileSize model.windowWidth newBoardSize
              }
            , Cmd.none
            )

        IncSize ->
            let
                newBoardSize =
                    min (model.boardSize + 1) maxBoardSize

                ( newBoard, newSeed ) =
                    GameBoard.initGameBoard newBoardSize model.seed
            in
            ( { model
                | boardSize = newBoardSize
                , board = newBoard
                , seed = newSeed
                , solved = False
                , tileSize = getTileSize model.windowWidth newBoardSize
              }
            , Cmd.none
            )


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
                [ gameWindowHeader model
                , gameWindow boardSize tiles
                ]
            )
        ]
    }


split : Int -> List a -> List (List a)
split toTake list =
    case list of
        [] ->
            []

        _ ->
            List.take toTake list :: split toTake (List.drop toTake list)


viewBody : List (Element msg) -> Element msg
viewBody contents =
    column
        [ width fill, height fill, Background.color darkBlue, Font.color white ]
        contents


gameWindow : Int -> List (Element Msg) -> Element Msg
gameWindow boardSize tiles =
    let
        rows =
            split boardSize tiles

        renderedRows =
            List.map (\x -> boardRow x) rows
    in
    column
        [ centerX
        , padding 40
        , Border.width TileSvg.borderWidth
        , Border.color darkBlue
        ]
        renderedRows


gameWindowHeader : Model -> Element Msg
gameWindowHeader model =
    row [ width fill, height (px 80), padding 60 ]
        [ row [ width (fillPortion 1), spacing 4 ]
            [ Input.button
                [ width shrink
                , height (px 60)
                , Font.center
                , padding 10
                , Border.rounded 6
                , Background.color lightBlue
                ]
                { onPress = Just GenerateSeed, label = text "New" }
            , decSizeButton model
            , incSizeButton model
            ]
        , el [ width (fillPortion 1), Font.center ]
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
                        { onPress = Just GenerateSeed, label = text "Play Again" }
                    ]

             else
                Element.none
            )
        , el [ width (fillPortion 1) ] Element.none
        ]


decSizeButton : Model -> Element Msg
decSizeButton model =
    if model.boardSize <= minBoardSize then
        Input.button
            [ width (px 60)
            , height (px 60)
            , Font.center
            , padding 10
            , Border.rounded 6
            , Background.color lightBlue
            , Region.description "board at min size"
            ]
            { onPress = Nothing, label = text "-" }

    else
        Input.button
            [ width (px 60)
            , height (px 60)
            , Font.center
            , padding 10
            , Border.rounded 6
            , Background.color lightBlue
            ]
            { onPress = Just DecSize, label = text "-" }


incSizeButton : Model -> Element Msg
incSizeButton model =
    if model.boardSize >= maxBoardSize then
        Input.button
            [ width (px 60)
            , height (px 60)
            , Font.center
            , padding 10
            , Border.rounded 6
            , Background.color lightBlue
            , Region.description "board at max size"
            ]
            { onPress = Nothing, label = text "+" }

    else
        Input.button
            [ width (px 60)
            , height (px 60)
            , Font.center
            , padding 10
            , Border.rounded 6
            , Background.color lightBlue
            ]
            { onPress = Just IncSize, label = text "+" }


boardRow : List (Element msg) -> Element msg
boardRow elements =
    row [ centerX, centerY ] elements


viewCell : Int -> GameBoard.Coords -> GameBoard.Cell -> Element Msg
viewCell tileSize coords cell =
    el
        [ centerX
        , centerY
        , Border.width TileSvg.borderWidth
        , Border.color darkBlue
        , Border.rounded 6
        , Background.color lightBlue
        , rotate (degrees (toFloat ((cell.initRotations + cell.rotations) * 90)))
        , Events.onClick (RotateTile coords)
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
