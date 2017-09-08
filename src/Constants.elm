module Constants exposing (..)

import Math.Vector2 exposing (vec2)
import Types exposing (Edge, Point)


level1 : { points : List Point, edges : List Edge }
level1 =
    { points =
        [ vec2 0 0
        , vec2 -400 -335
        , vec2 -350 270
        , vec2 200 -250
        , vec2 30 320
        ]
    , edges =
        [ Edge 0 1
        , Edge 0 2
        , Edge 1 2
        , Edge 2 3
        , Edge 3 4
        , Edge 0 4
        , Edge 1 4
        ]
    }


pointRadius : Float
pointRadius =
    8
