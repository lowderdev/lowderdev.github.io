module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Debug exposing (toString)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



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


view : Model -> Html Msg
view { grid } =
    div
        [ style "margin-top" "2em"
        , style "border" "red dashed 1px"
        ]
        [ viewGrid grid ]


viewGrid : Grid -> Html Msg
viewGrid grid =
    div
        [ style "display" "grid"
        , style "justify-content" "center"
        , style "grid-template-columns" "100px 50px 50px 100px"
        , style "grid-template-rows" "auto"
        , style "grid-template-areas"
            ("grid-margin"
                ++ ". row1 row1 ."
                ++ ". row2 row2 ."
            )
        ]
        (List.map
            (\row ->
                div []
                    (List.indexedMap
                        (\i rowValue ->
                            div
                                [ style "grid-area" ("row" ++ toString i)
                                , style "height" "100px"
                                , style "width" "100px"
                                , style "margin" "1em auto"
                                , style "border" "red dashed 1px"
                                , style "text-align" "center"
                                ]
                                [ text rowValue ]
                        )
                        row
                    )
            )
            grid
        )
