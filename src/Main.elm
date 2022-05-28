module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Css
import Css.Global
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attr
import Tailwind.Utilities as Tw



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view >> toUnstyled
        }



-- MODEL


type alias Cell =
    -- four bool fields to represent whether that connection is active or not
    { connections : List Bool, rotations : Int }


type alias Row =
    List Cell


type alias Grid =
    List Row


type alias Model =
    { grid : Grid }


init : Model
init =
    let
        a =
            [ { connections = [ True, True, False, False ], rotations = 0 } ]

        b =
            [ { connections = [ True, True, False, False ], rotations = 0 } ]

        c =
            [ { connections = [ True, True, False, False ], rotations = 0 } ]

        d =
            [ { connections = [ True, True, False, False ], rotations = 0 } ]
    in
    { grid = [ a, b, c, d ] }



-- UPDATE


type Msg
    = Reset


update : Msg -> Model -> Model
update msg { grid } =
    case msg of
        Reset ->
            { grid = grid }



-- VIEW


view : Model -> Html msg
view { grid } =
    div [ Attr.css [ Tw.bg_gray_50, Tw.rounded_t_xl, Tw.p_24 ] ]
        [ -- This will give us the standard tailwind style-reset as well as the fonts
          Css.Global.global Tw.globalStyles
        , viewBoard grid
        ]


viewBoard : Grid -> Html msg
viewBoard grid =
    div
        [ Attr.css [ Tw.bg_blue_100, Tw.grid, Tw.grid_cols_2, Tw.w_24 ] ]
        (List.map
            (\row ->
                div []
                    (List.map
                        (\cell ->
                            viewCell cell
                        )
                        row
                    )
            )
            grid
        )


viewCell : Cell -> Html msg
viewCell { connections, rotations } =
    case connections of
        [ False, False, False, False ] ->
            cellDiv

        [ True, False, False, False ] ->
            "l"

        _ ->
            " "


cellDiv : Html msg
cellDiv =
    div
        [ Attr.css [ Tw.bg_blue_500, Tw.rounded_sm, Tw.w_12, Tw.h_12, Tw.text_center ] ]
        [ text (showCell cell) ]
