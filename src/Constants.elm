module Constants exposing (edgesForLevel, pointsForLevel, pointRadius)

import Array exposing (Array)
import Dict exposing (Dict)
import Math.Vector2 exposing (vec2)
import Types exposing (Edge, EdgeDict, PointDict)


type alias Level =
    { points : PointDict, edges : EdgeDict }


pointsForLevel : Int -> PointDict
pointsForLevel n =
    Array.get (n - 1) levels
        |> Maybe.withDefault level1
        |> .points


edgesForLevel : Int -> EdgeDict
edgesForLevel n =
    Array.get (n - 1) levels
        |> Maybe.withDefault level1
        |> .edges


levels : Array Level
levels =
    Array.fromList
        [ level1
        , level2
        ]


level1 : Level
level1 =
    { points =
        Dict.fromList
            [ ( "0", vec2 0 0 )
            , ( "1", vec2 -400 -335 )
            , ( "2", vec2 -350 270 )
            , ( "3", vec2 200 -250 )
            , ( "4", vec2 30 320 )
            ]
    , edges =
        Dict.fromList
            [ ( "0", Edge "0" "1" )
            , ( "1", Edge "0" "2" )
            , ( "2", Edge "1" "2" )
            , ( "3", Edge "2" "3" )
            , ( "4", Edge "3" "4" )
            , ( "5", Edge "0" "4" )
            , ( "6", Edge "1" "4" )
            ]
    }


level2 : Level
level2 =
    { points =
        Dict.fromList
            [ ( "0", vec2 0 0 )
            , ( "1", vec2 -200 0 )
            , ( "2", vec2 -250 75 )
            , ( "3", vec2 -100 125 )
            , ( "4", vec2 0 50 )
            , ( "5", vec2 30 270 )
            , ( "6", vec2 175 -120 )
            ]
    , edges =
        Dict.fromList
            [ ( "0", Edge "0" "1" )
            , ( "1", Edge "0" "2" )
            , ( "2", Edge "1" "2" )
            , ( "3", Edge "2" "3" )
            , ( "4", Edge "3" "4" )
            , ( "5", Edge "0" "4" )
            , ( "6", Edge "1" "4" )
            , ( "7", Edge "3" "5" )
            , ( "8", Edge "0" "5" )
            , ( "9", Edge "2" "6" )
            , ( "10", Edge "1" "6" )
            ]
    }


pointRadius : Float
pointRadius =
    8
