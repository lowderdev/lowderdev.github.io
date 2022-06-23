module TileSvg exposing
    ( barSvg
    , borderWidth
    , elbowSvg
    , knobSvg
    , teeSvg
    , tileWidth
    )

import Element exposing (Element, html)
import Svg exposing (Svg)
import Svg.Attributes exposing (..)



-- These three integers, tileInnerWidth, strokeWidth, and borderWidth, should be the only
-- params needed to change to resize the tile and the tile shapes.
-- tileInnerWidth and strokeWidth should be evenly divisible by 2
--
-- Colors:
-- navy (rgb255 6 40 61)
-- blue (rgb255 19 99 223)
--
-- tileInnerWidth : Int
-- tileInnerWidth =
--     120


strokeWidth : Int -> Int
strokeWidth tileSize =
    tileSize // 3 |> roundToEven


strokeColor : Svg.Attribute msg
strokeColor =
    color "rgba(255, 255, 255, 255)"


borderWidth : Int
borderWidth =
    1


halfWidthS : Int -> String
halfWidthS tileSize =
    String.fromInt <| tileSize // 2


tileInnerWidthS : Int -> String
tileInnerWidthS tileSize =
    String.fromInt tileSize


tileWidth : Int -> Int
tileWidth tileSize =
    tileSize + (borderWidth * 2)


strokeWidthS : Int -> String
strokeWidthS tileSize =
    String.fromInt (strokeWidth tileSize)


strokeWidthOffset : Int -> Int
strokeWidthOffset tileSize =
    strokeWidth tileSize // 2


centerStrokeS : Int -> String
centerStrokeS tileSize =
    String.fromInt ((tileSize // 2) - strokeWidthOffset tileSize)


roundToEven : Int -> Int
roundToEven int =
    2 * (int // 2)


tileViewBox : Int -> Svg.Attribute msg
tileViewBox tileSize =
    viewBox ("0 0 " ++ tileInnerWidthS tileSize ++ " " ++ tileInnerWidthS tileSize)


knobSvg : Int -> Element msg
knobSvg tileSize =
    styledSvg tileSize
        [ Svg.circle
            [ cx (halfWidthS tileSize)
            , cy (halfWidthS tileSize)
            , r (String.fromFloat (toFloat (strokeWidth tileSize) * 0.8))
            , fill "currentcolor"
            ]
            []
        , Svg.rect
            [ x (centerStrokeS tileSize)
            , y "0"
            , width (strokeWidthS tileSize)
            , height (halfWidthS tileSize)
            , fill "currentcolor"
            ]
            []
        ]


elbowSvg : Int -> Element msg
elbowSvg tileSize =
    styledSvg tileSize
        [ Svg.rect
            [ x (centerStrokeS tileSize)
            , y "0"
            , width (strokeWidthS tileSize)
            , height (halfWidthS tileSize)
            , fill "currentcolor"
            ]
            []
        , Svg.rect
            [ x (halfWidthS tileSize)
            , y (centerStrokeS tileSize)
            , width (halfWidthS tileSize)
            , height (strokeWidthS tileSize)
            , fill "currentcolor"
            ]
            []
        , Svg.circle
            [ cx (halfWidthS tileSize)
            , cy (halfWidthS tileSize)
            , r (String.fromInt (strokeWidthOffset tileSize))
            , fill "currentcolor"
            ]
            []
        ]


barSvg : Int -> Element msg
barSvg tileSize =
    styledSvg tileSize
        [ Svg.rect
            [ x (centerStrokeS tileSize)
            , y "0"
            , width (strokeWidthS tileSize)
            , height (tileInnerWidthS tileSize)
            , fill "currentcolor"
            ]
            []
        ]


teeSvg : Int -> Element msg
teeSvg tileSize =
    styledSvg tileSize
        [ Svg.rect
            [ x (centerStrokeS tileSize)
            , y "0"
            , width (strokeWidthS tileSize)
            , height (tileInnerWidthS tileSize)
            , fill "currentcolor"
            ]
            []
        , Svg.rect
            [ x (halfWidthS tileSize)
            , y (centerStrokeS tileSize)
            , width (halfWidthS tileSize)
            , height (strokeWidthS tileSize)
            , fill "currentcolor"
            ]
            []
        ]


styledSvg : Int -> List (Svg msg) -> Element msg
styledSvg tileSize elements =
    html <|
        Svg.svg
            [ width (tileInnerWidthS tileSize)
            , height (tileInnerWidthS tileSize)
            , tileViewBox tileSize
            , strokeColor
            ]
            elements
