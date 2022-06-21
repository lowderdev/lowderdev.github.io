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
import Model exposing (Cell, Coords, Model, Shape(..), initGameState)
import Random
import TileSvg exposing (barSvg, borderWidth, elbowSvg, knobSvg, teeSvg, tileWidth)


defaultBoardSize : Int
defaultBoardSize =
    4


main : Program Int Model Msg
main =
    Browser.document
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


init : Int -> ( Model, Cmd Msg )
init int =
    ( initGameState
        { boardSize = defaultBoardSize
        , board = Dict.empty
        , seed = Random.initialSeed int
        , solved = False
        }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ boardSize, board } as model) =
    case msg of
        Reset ->
            ( model, Cmd.none )

        GenerateSeed ->
            ( model, generateSeed )

        NewSeed int ->
            ( initGameState { model | seed = Random.initialSeed int }, Cmd.none )

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
            ( { model | board = newBoard, solved = isSolved }, Cmd.none )

        DecSize ->
            let
                minSize =
                    3

                newBoardSize =
                    max minSize (boardSize - 1)
            in
            ( initGameState { model | boardSize = newBoardSize }, generateSeed )

        IncSize ->
            let
                maxSize =
                    10

                newBoardSize =
                    min (boardSize + 1) maxSize
            in
            ( initGameState { model | boardSize = newBoardSize }, generateSeed )


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
            , Background.color (rgb255 32 83 117)
            ]
            { onPress = Just GenerateSeed, label = text "🔄" }
        ]


decSizeButton : Model -> Element Msg
decSizeButton model =
    if model.boardSize <= 3 then
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


incSizeButton : Model -> Element Msg
incSizeButton model =
    if model.boardSize >= 10 then
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
