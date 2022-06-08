module TileSvg exposing (..)

import Element exposing (Element, html)
import Svg exposing (..)
import Svg.Attributes exposing (..)


knobSvg : Element msg
knobSvg =
    html
        (svg
            [ width "60"
            , height "60"
            , viewBox "0 0 60 60"
            ]
            [ circle
                [ cx "30"
                , cy "30"
                , r "10"
                ]
                []
            , rect
                [ x "30"
                , y "25"
                , width "30"
                , height "10"
                ]
                []
            ]
        )


elbowSvg : Element msg
elbowSvg =
    html
        (Svg.svg
            [ width "60"
            , height "60"
            , viewBox "0 0 60 60"
            ]
            [ rect
                [ x "25"
                , y "30"
                , width "10"
                , height "30"
                ]
                []
            , rect
                [ x "30"
                , y "25"
                , width "30"
                , height "10"
                ]
                []
            , circle
                [ cx "30"
                , cy "30"
                , r "5"
                ]
                []
            ]
        )


barSvg : Element msg
barSvg =
    html
        (Svg.svg
            [ width "60"
            , height "60"
            , viewBox "0 0 60 60"
            ]
            [ rect
                [ x "0"
                , y "25"
                , width "60"
                , height "10"
                ]
                []
            ]
        )


teeSvg : Element msg
teeSvg =
    html
        (Svg.svg
            [ width "60"
            , height "60"
            , viewBox "0 0 60 60"
            ]
            [ rect
                [ x "0"
                , y "25"
                , width "60"
                , height "10"
                ]
                []
            , rect
                [ x "25"
                , y "30"
                , width "10"
                , height "30"
                ]
                []
            ]
        )
