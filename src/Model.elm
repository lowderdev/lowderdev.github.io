module Model exposing (..)

import Dict exposing (Dict)


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


initGameState : Model
initGameState =
    let
        a =
            { shape = Knob, rotations = 0 }

        b =
            { shape = Bar, rotations = 0 }

        c =
            { shape = Elbow, rotations = 0 }

        d =
            { shape = Tee, rotations = 0 }
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
