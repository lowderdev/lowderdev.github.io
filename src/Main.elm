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
import Tailwind.Breakpoints as Breakpoints
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


type alias Grid =
    List (List String)


type alias Model =
    { grid : Grid }


init : Model
init =
    { grid = [ [ "-", "-" ], [ "|", "|" ] ] }



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
        , div
            [ Attr.css [ Tw.bg_blue_100, Tw.grid, Tw.grid_cols_2, Tw.w_24 ] ]
            (List.map
                (\row ->
                    div []
                        (List.map
                            (\rowValue ->
                                div
                                    [ Attr.css [ Tw.bg_blue_500, Tw.rounded_sm, Tw.w_12, Tw.h_12, Tw.text_center ] ]
                                    [ text rowValue ]
                            )
                            row
                        )
                )
                grid
            )
        , div []
            [ a
                [ Attr.css
                    [ Tw.inline_flex
                    , Tw.items_center
                    , Tw.justify_center
                    , Tw.px_5
                    , Tw.py_3
                    , Tw.border
                    , Tw.border_transparent
                    , Tw.text_base
                    , Tw.font_medium
                    , Tw.rounded_md
                    , Tw.text_white
                    , Tw.bg_indigo_600

                    -- We can use hover styles via elm-css :)
                    , Css.hover [ Tw.bg_indigo_700 ]
                    ]
                , Attr.href "#"
                ]
                [ text "Get started" ]
            ]
        ]



-- viewBoard : Grid -> Html msg
-- viewBoard grid =
--   div [] [
--   ]
