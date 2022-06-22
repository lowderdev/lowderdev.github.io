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


tileInnerWidth : Int
tileInnerWidth =
    120


strokeWidth : Int
strokeWidth =
    40


strokeColor : Svg.Attribute msg
strokeColor =
    color "rgba(255, 255, 255, 255)"


borderWidth : Int
borderWidth =
    1


halfWidthS : String
halfWidthS =
    String.fromInt <| tileInnerWidth // 2


tileInnerWidthS : String
tileInnerWidthS =
    String.fromInt tileInnerWidth


tileWidth : Int
tileWidth =
    tileInnerWidth + (borderWidth * 2)


strokeWidthS : String
strokeWidthS =
    String.fromInt strokeWidth


strokeWidthOffset : Int
strokeWidthOffset =
    strokeWidth // 2


centerStrokeS : String
centerStrokeS =
    String.fromInt ((tileInnerWidth // 2) - strokeWidthOffset)


tileViewBox : Svg.Attribute msg
tileViewBox =
    viewBox ("0 0 " ++ tileInnerWidthS ++ " " ++ tileInnerWidthS)


knobSvg : Element msg
knobSvg =
    styledSvg
        [ Svg.circle
            [ cx halfWidthS
            , cy halfWidthS
            , r (String.fromFloat (toFloat strokeWidth * 0.8))
            , fill "currentcolor"
            ]
            []
        , Svg.rect
            [ x centerStrokeS
            , y "0"
            , width strokeWidthS
            , height halfWidthS
            , fill "currentcolor"
            ]
            []
        ]


elbowSvg : Element msg
elbowSvg =
    styledSvg
        [ Svg.rect
            [ x centerStrokeS
            , y "0"
            , width strokeWidthS
            , height halfWidthS
            , fill "currentcolor"
            ]
            []
        , Svg.rect
            [ x halfWidthS
            , y centerStrokeS
            , width halfWidthS
            , height strokeWidthS
            , fill "currentcolor"
            ]
            []
        , Svg.circle
            [ cx halfWidthS
            , cy halfWidthS
            , r (String.fromInt strokeWidthOffset)
            , fill "currentcolor"
            ]
            []
        ]


barSvg : Element msg
barSvg =
    styledSvg
        [ Svg.rect
            [ x centerStrokeS
            , y "0"
            , width strokeWidthS
            , height tileInnerWidthS
            , fill "currentcolor"
            ]
            []
        ]


teeSvg : Element msg
teeSvg =
    styledSvg
        [ Svg.rect
            [ x centerStrokeS
            , y "0"
            , width strokeWidthS
            , height tileInnerWidthS
            , fill "currentcolor"
            ]
            []
        , Svg.rect
            [ x halfWidthS
            , y centerStrokeS
            , width halfWidthS
            , height strokeWidthS
            , fill "currentcolor"
            ]
            []
        ]


styledSvg : List (Svg msg) -> Element msg
styledSvg elements =
    html <|
        Svg.svg
            [ width tileInnerWidthS
            , height tileInnerWidthS
            , tileViewBox
            , strokeColor
            ]
            elements
