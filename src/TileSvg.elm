module TileSvg exposing (..)

import Debug exposing (toString)
import Element exposing (Element, html)
import Svg exposing (..)
import Svg.Attributes exposing (..)



-- These three integers, tileInnerWidth, strokeWidth, and borderWidth, should be the only
-- params needed to change to resize the tile and the tile shapes.
-- tileInnerWidth and strokeWidth should be evenly divisible by 2


tileInnerWidth : Int
tileInnerWidth =
    120


strokeWidth : Int
strokeWidth =
    20


borderWidth : Int
borderWidth =
    1


halfWidthS : String
halfWidthS =
    toString (tileInnerWidth // 2)


tileInnerWidthS : String
tileInnerWidthS =
    toString tileInnerWidth


tileWidth : Int
tileWidth =
    tileInnerWidth + (borderWidth * 2)


strokeWidthS : String
strokeWidthS =
    toString strokeWidth


strokeWidthOffset : Int
strokeWidthOffset =
    strokeWidth // 2


centerStrokeS : String
centerStrokeS =
    toString ((tileInnerWidth // 2) - strokeWidthOffset)


tileViewBox : Attribute msg
tileViewBox =
    viewBox ("0 0 " ++ tileInnerWidthS ++ " " ++ tileInnerWidthS)


knobSvg : Element msg
knobSvg =
    html
        (svg
            [ width tileInnerWidthS
            , height tileInnerWidthS
            , tileViewBox
            ]
            [ circle
                [ cx halfWidthS
                , cy halfWidthS
                , r strokeWidthS
                ]
                []
            , rect
                [ x halfWidthS
                , y centerStrokeS
                , width halfWidthS
                , height strokeWidthS
                ]
                []
            ]
        )


elbowSvg : Element msg
elbowSvg =
    html
        (Svg.svg
            [ width tileInnerWidthS
            , height tileInnerWidthS
            , tileViewBox
            ]
            [ rect
                [ x centerStrokeS
                , y halfWidthS
                , width strokeWidthS
                , height halfWidthS
                ]
                []
            , rect
                [ x halfWidthS
                , y centerStrokeS
                , width halfWidthS
                , height strokeWidthS
                ]
                []
            , circle
                [ cx halfWidthS
                , cy halfWidthS
                , r (toString strokeWidthOffset)
                ]
                []
            ]
        )


barSvg : Element msg
barSvg =
    html
        (Svg.svg
            [ width tileInnerWidthS
            , height tileInnerWidthS
            , tileViewBox
            ]
            [ rect
                [ x "0"
                , y centerStrokeS
                , width tileInnerWidthS
                , height strokeWidthS
                ]
                []
            ]
        )


teeSvg : Element msg
teeSvg =
    html
        (Svg.svg
            [ width tileInnerWidthS
            , height tileInnerWidthS
            , tileViewBox
            ]
            [ rect
                [ x "0"
                , y centerStrokeS
                , width tileInnerWidthS
                , height strokeWidthS
                ]
                []
            , rect
                [ x centerStrokeS
                , y halfWidthS
                , width strokeWidthS
                , height halfWidthS
                ]
                []
            ]
        )
