module Main exposing (main)

import Browser
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


main : Program Int Model Msg
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
    }


init : Int -> ( Model, Cmd Msg )
init int =
    let
        ( board, seed ) =
            GameBoard.initGameBoard defaultBoardSize (Random.initialSeed int)
    in
    ( { boardSize = defaultBoardSize
      , board = board
      , seed = seed
      , solved = False
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = GenerateSeed
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
            ( { model | boardSize = newBoardSize, board = newBoard, seed = newSeed, solved = False }, Cmd.none )

        IncSize ->
            let
                newBoardSize =
                    min (model.boardSize + 1) maxBoardSize

                ( newBoard, newSeed ) =
                    GameBoard.initGameBoard newBoardSize model.seed
            in
            ( { model | boardSize = newBoardSize, board = newBoard, seed = newSeed, solved = False }, Cmd.none )


view : Model -> Browser.Document Msg
view ({ board } as model) =
    let
        widthHeight =
            round <| sqrt <| toFloat <| Dict.size <| board

        tiles =
            Dict.foldr (\k v acc -> viewCell k v :: acc) [] board

        rows =
            split widthHeight tiles
    in
    { title = "Graph Bang"
    , body =
        [ layout []
            (body
                [ gameWindowHeader model
                , gameWindow (List.map (\x -> boardRow x) rows)
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


body : List (Element msg) -> Element msg
body contents =
    column
        [ width fill, height fill, Background.color darkBlue, Font.color white ]
        contents


gameWindow : List (Element Msg) -> Element Msg
gameWindow contents =
    column
        [ centerX
        , centerY
        , Border.width TileSvg.borderWidth
        , Border.color
            darkBlue
        ]
        contents


gameWindowHeader : Model -> Element Msg
gameWindowHeader model =
    row [ width fill, height (px 80), spacing 4, padding 50 ]
        [ paragraph []
            [ text
                (if model.solved then
                    "Solved!"

                 else
                    ""
                )
            ]
        , decSizeButton model
        , incSizeButton model
        , Input.button
            [ width (px 40)
            , Font.center
            , padding 10
            , Border.rounded 6
            , Background.color lightBlue
            ]
            { onPress = Just GenerateSeed, label = text "🔄" }
        ]


decSizeButton : Model -> Element Msg
decSizeButton model =
    if model.boardSize <= minBoardSize then
        Input.button
            [ width (px 40)
            , Font.center
            , padding 10
            , Border.rounded 6
            , Background.color lightBlue
            , Region.description "board at min size"
            ]
            { onPress = Nothing, label = text "-" }

    else
        Input.button
            [ width (px 40)
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
            [ width (px 40)
            , Font.center
            , padding 10
            , Border.rounded 6
            , Background.color lightBlue
            , Region.description "board at max size"
            ]
            { onPress = Nothing, label = text "+" }

    else
        Input.button
            [ width (px 40)
            , Font.center
            , padding 10
            , Border.rounded 6
            , Background.color lightBlue
            ]
            { onPress = Just IncSize, label = text "+" }


boardRow : List (Element msg) -> Element msg
boardRow elements =
    row [ centerX, centerY ] elements


viewCell : GameBoard.Coords -> GameBoard.Cell -> Element Msg
viewCell coords cell =
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
                TileSvg.knobSvg

            Bar ->
                TileSvg.barSvg

            Elbow ->
                TileSvg.elbowSvg

            Tee ->
                TileSvg.teeSvg

            Empty ->
                Element.none
        )
