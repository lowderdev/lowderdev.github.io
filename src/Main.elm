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
import Html exposing (Html)
import Maybe exposing (andThen)
import Model exposing (Cell, Coords, GameState, Shape(..), initGameState)
import Random
import TileSvg exposing (barSvg, borderWidth, elbowSvg, knobSvg, teeSvg, tileWidth)


defaultBoardSize : Int
defaultBoardSize =
    4


main : Program () GameState Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : () -> ( GameState, Cmd Msg )
init _ =
    ( initGameState
        { boardSize = defaultBoardSize
        , board = Dict.empty
        , seed = Random.initialSeed 1
        , solved = False
        }
    , generateSeed
    )


subscriptions : GameState -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = Reset
    | GenerateSeed
    | NewSeed Int
    | RotateTile Coords
    | DecSize
    | IncSize


generateSeed : Cmd Msg
generateSeed =
    Random.generate NewSeed (Random.int Random.minInt Random.maxInt)


update : Msg -> GameState -> ( GameState, Cmd Msg )
update msg ({ boardSize, board } as gameState) =
    case msg of
        Reset ->
            ( gameState, Cmd.none )

        GenerateSeed ->
            ( gameState, generateSeed )

        NewSeed int ->
            ( initGameState { gameState | seed = Random.initialSeed int }, Cmd.none )

        RotateTile coords ->
            let
                newBoard =
                    Dict.update
                        (Debug.log "coords" coords)
                        (andThen (\cell -> Just { cell | rotations = remainderBy 4 (cell.rotations + 1) }))
                        board

                isSolved =
                    Debug.log "solved" <|
                        List.all
                            (\x -> x == 0)
                            (Debug.log "board" (List.map (\{ rotations } -> rotations) (Dict.values newBoard)))
            in
            ( { gameState | board = newBoard, solved = isSolved }, Cmd.none )

        DecSize ->
            let
                minSize =
                    3

                newBoardSize =
                    max minSize (boardSize - 1)
            in
            ( initGameState { gameState | boardSize = newBoardSize }, generateSeed )

        IncSize ->
            let
                maxSize =
                    10

                newBoardSize =
                    min (boardSize + 1) maxSize
            in
            ( initGameState { gameState | boardSize = newBoardSize }, generateSeed )


view : GameState -> Html Msg
view ({ board } as gameState) =
    let
        widthHeight =
            round <| sqrt <| toFloat <| Dict.size <| board

        tiles =
            Dict.foldr (\k v acc -> viewCell k v :: acc) [] board

        rows =
            split widthHeight tiles
    in
    layout []
        (body
            [ gameWindowHeader gameState
            , gameWindow (List.map (\x -> boardRow x) rows)
            ]
        )


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
        [ width fill, height fill, Background.color (rgb255 17 43 60), Font.color (rgb 1 1 1) ]
        contents


gameWindow : List (Element Msg) -> Element Msg
gameWindow contents =
    column
        [ centerX
        , centerY
        , Border.width borderWidth
        , Border.color
            (rgb255 17 43 60)
        ]
        contents


gameWindowHeader : GameState -> Element Msg
gameWindowHeader gameState =
    row [ width fill, height (px 80), spacing 4, padding 50 ]
        [ paragraph []
            [ text
                (if gameState.solved then
                    "Solved!"

                 else
                    ""
                )
            ]
        , decSizeButton gameState
        , incSizeButton gameState
        , Input.button
            [ width (px 40)
            , Font.center
            , padding 10
            , Border.rounded 6
            , Background.color (rgb255 32 83 117)
            ]
            { onPress = Just GenerateSeed, label = text "🔄" }
        ]


decSizeButton : GameState -> Element Msg
decSizeButton gameState =
    if gameState.boardSize <= 3 then
        Input.button
            [ width (px 40)
            , Font.center
            , padding 10
            , Border.rounded 6
            , Background.color (rgb255 122 135 157)
            , Region.description "board at min size"
            ]
            { onPress = Nothing, label = text "-" }

    else
        Input.button
            [ width (px 40)
            , Font.center
            , padding 10
            , Border.rounded 6
            , Background.color (rgb255 32 83 117)
            ]
            { onPress = Just DecSize, label = text "-" }


incSizeButton : GameState -> Element Msg
incSizeButton gameState =
    if gameState.boardSize >= 10 then
        Input.button
            [ width (px 40)
            , Font.center
            , padding 10
            , Border.rounded 6
            , Background.color (rgb255 122 135 157)
            , Region.description "board at max size"
            ]
            { onPress = Nothing, label = text "+" }

    else
        Input.button
            [ width (px 40)
            , Font.center
            , padding 10
            , Border.rounded 6
            , Background.color (rgb255 32 83 117)
            ]
            { onPress = Just IncSize, label = text "+" }


boardRow : List (Element msg) -> Element msg
boardRow elements =
    row [ centerX, centerY ] elements


viewCell : Coords -> Cell -> Element Msg
viewCell coords cell =
    el
        [ width (px tileWidth)
        , height (px tileWidth)
        , centerX
        , centerY
        , Border.width borderWidth
        , Border.color (rgb255 17 43 60)
        , Border.rounded 6
        , Background.color (rgb255 32 83 117)
        , rotate (degrees (toFloat ((cell.initRotations + cell.rotations) * 90)))
        , Events.onClick (RotateTile coords)
        ]
        (case cell.shape of
            Knob ->
                knobSvg

            Bar ->
                barSvg

            Elbow ->
                elbowSvg

            Tee ->
                teeSvg

            Empty ->
                Element.none
        )
