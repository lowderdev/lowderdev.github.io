module Model exposing (..)

import Array exposing (fromList)
import Dict exposing (Dict)
import Maybe exposing (andThen, withDefault)


type alias Model =
    Dict Coords Cell


type alias Coords =
    ( Int, Int )


type alias Cell =
    { shape : Shape, rotations : Int }


type Shape
    = Knob
    | Bar
    | Elbow
    | Tee
    | Empty


getShape : List ( Int, Int ) -> Int -> Shape
getShape ints index =
    ints
        |> fromList
        |> Array.get index
        |> andThen (\p -> Just (Tuple.first p))
        |> andThen (\n -> Just (intToShape n))
        |> withDefault Empty


getRotation : List ( Int, Int ) -> Int -> Int
getRotation ints index =
    ints
        |> fromList
        |> Array.get index
        |> andThen (\p -> Just (Tuple.second p))
        |> withDefault -1


intToShape : Int -> Shape
intToShape int =
    case int of
        0 ->
            Knob

        1 ->
            Bar

        2 ->
            Elbow

        3 ->
            Tee

        _ ->
            Empty


initGameState : List ( Int, Int ) -> Model
initGameState ints =
    let
        a =
            { shape = getShape ints 0, rotations = getRotation ints 0 }

        b =
            { shape = getShape ints 1, rotations = getRotation ints 1 }

        c =
            { shape = getShape ints 2, rotations = getRotation ints 2 }

        d =
            { shape = getShape ints 3, rotations = getRotation ints 3 }
    in
    Dict.fromList
        [ ( ( 0, 0 ), a )
        , ( ( 0, 1 ), b )
        , ( ( 1, 0 ), c )
        , ( ( 1, 1 ), d )
        ]


emptyCell : Cell
emptyCell =
    { shape = Empty, rotations = 0 }
