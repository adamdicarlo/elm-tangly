module Levels exposing (edgesForLevel, pointsForLevel)

import Dict
import List.Extra as List
import Math.Vector2 exposing (vec2)
import Types exposing (Edge, EdgeDict, PointDict)


type alias Level =
    { points : PointDict, edges : EdgeDict }


pointsForLevel : Int -> PointDict
pointsForLevel n =
    List.getAt (n - 1) levels
        |> Maybe.withDefault level1
        |> .points


edgesForLevel : Int -> EdgeDict
edgesForLevel n =
    List.getAt (n - 1) levels
        |> Maybe.withDefault level1
        |> .edges


levels : List Level
levels =
    [ level1
    , level2
    , level3
    , level4
    , level5
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


level3 : Level
level3 =
    { edges =
        Dict.fromList
            [ ( "6", Edge "1" "4" )
            , ( "5", Edge "0" "4" )
            , ( "4", Edge "3" "4" )
            , ( "3", Edge "2" "3" )
            , ( "2", Edge "1" "2" )
            , ( "1", Edge "0" "2" )
            , ( "0", Edge "0" "1" )
            , ( "4-5", Edge "4" "5" )
            , ( "3-5", Edge "3" "5" )
            , ( "2-5", Edge "2" "5" )
            , ( "1-3", Edge "1" "3" )
            , ( "0-5", Edge "0" "5" )
            ]
    , points =
        Dict.fromList
            [ ( "5", vec2 244 121.5 )
            , ( "4", vec2 72 77.5 )
            , ( "3", vec2 -102 341.5 )
            , ( "2", vec2 101 309.5 )
            , ( "1", vec2 -80 217.5 )
            , ( "0", vec2 -78 116.5 )
            ]
    }


level4 : Level
level4 =
    { edges =
        Dict.fromList
            [ ( "6", Edge "1" "4" )
            , ( "5", Edge "0" "4" )
            , ( "4", Edge "3" "4" )
            , ( "3", Edge "2" "3" )
            , ( "2", Edge "1" "2" )
            , ( "1", Edge "0" "2" )
            , ( "0", Edge "0" "1" )
            , ( "7", Edge "abc" "f0a" )
            , ( "8", Edge "quux" "abc" )
            , ( "9", Edge "quux" "@@@" )
            , ( "10", Edge "4" "f0a" )
            , ( "11", Edge "4" "abc" )
            , ( "12", Edge "4" "f0a" )
            , ( "13", Edge "2" "f0a" )
            , ( "14", Edge "2" "abc" )
            , ( "15", Edge "2" "*" )
            , ( "16", Edge "2" "@@@" )
            , ( "17", Edge "2" "quux" )
            , ( "18", Edge "1" "*" )
            , ( "19", Edge "1" "@@@" )
            , ( "20", Edge "1" "quux" )
            , ( "21", Edge "0" "*" )
            ]
    , points =
        Dict.fromList
            [ ( "f0a", vec2 -57 146.5 )
            , ( "abc", vec2 -276 -137.5 )
            , ( "*", vec2 -119 17.5 )
            , ( "@@@", vec2 -285 -12.5 )
            , ( "quux", vec2 -124 -14.5 )
            , ( "4", vec2 10 -41.5 )
            , ( "3", vec2 -51 187.5 )
            , ( "2", vec2 137 -97.5 )
            , ( "1", vec2 -311 238.5 )
            , ( "0", vec2 -319 -119.5 )
            ]
    }


level5 : Level
level5 =
    { edges =
        Dict.fromList
            [ ( "p245,158-p326,-41", Edge "p245,158" "p326,-41" )
            , ( "p-7,-231-p326,-41", Edge "p-7,-231" "p326,-41" )
            , ( "p-7,-231-p245,158", Edge "p-7,-231" "p245,158" )
            , ( "6", Edge "1" "4" )
            , ( "5", Edge "0" "4" )
            , ( "4-p326,-41", Edge "4" "p326,-41" )
            , ( "4", Edge "3" "4" )
            , ( "3-p326,-41", Edge "3" "p326,-41" )
            , ( "3-p245,158", Edge "3" "p245,158" )
            , ( "3", Edge "2" "3" )
            , ( "2-p245,158", Edge "2" "p245,158" )
            , ( "2-p-7,-231", Edge "2" "p-7,-231" )
            , ( "2", Edge "1" "2" )
            , ( "1-3", Edge "1" "3" )
            , ( "1", Edge "0" "2" )
            , ( "0-p326,-41", Edge "0" "p326,-41" )
            , ( "0-p-7,-231", Edge "0" "p-7,-231" )
            , ( "0", Edge "0" "1" )
            ]
    , points =
        Dict.fromList
            [ ( "p326,-41", vec2 -114 8 )
            , ( "p245,158", vec2 -70 -11 )
            , ( "p-7,-231", vec2 -51 242 )
            , ( "4", vec2 20 251 )
            , ( "3", vec2 -171 205 )
            , ( "2", vec2 8 -57 )
            , ( "1", vec2 -6 149 )
            , ( "0", vec2 -171 304 )
            ]
    }
