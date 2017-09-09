module Constants exposing (edgesForLevel, pointsForLevel, pointRadius)

import Array exposing (Array)
import Math.Vector2 exposing (vec2)
import Types exposing (Edge, Point)


type alias Level =
    { points : List Point, edges : List Edge }


pointsForLevel : Int -> List Point
pointsForLevel n =
    (Array.get (n - 1) levels) |> Maybe.withDefault level1 |> .points


edgesForLevel : Int -> List Edge
edgesForLevel n =
    (Array.get (n - 1) levels) |> Maybe.withDefault level1 |> .edges


levels : Array Level
levels =
    Array.fromList
        [ level1
        , level2
        ]


level1 : Level
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


level2 : Level
level2 =
    { points =
        [ vec2 0 0
        , vec2 -200 0
        , vec2 -250 75
        , vec2 -100 125
        , vec2 0 50
        , vec2 30 270
        , vec2 175 -120
        ]
    , edges =
        [ Edge 0 1
        , Edge 0 2
        , Edge 1 2
        , Edge 2 3
        , Edge 3 4
        , Edge 0 4
        , Edge 1 4
        , Edge 3 5
        , Edge 0 5
        , Edge 2 6
        , Edge 1 6
        ]
    }


pointRadius : Float
pointRadius =
    8
