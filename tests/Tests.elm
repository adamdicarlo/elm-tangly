module Tests exposing (..)

import Dict
import String
import Expect
import Math.Vector2 exposing (fromTuple, vec2)
import Test exposing (Test, describe, test, todo, skip, only)
import Edge exposing (allIntersections, intersect, nearlyEqual)
import Types exposing (Edge)


intersectVerticalCollinearSegmentsTests : Test
intersectVerticalCollinearSegmentsTests =
    describe "intersect with vertical segments"
        [ test "that intersect, first below second" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 -5 5 ), ( "1", vec2 -5 0 ), ( "2", vec2 -5 8 ), ( "3", vec2 -5 4 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Just (vec2 -5 4.5)
        , test "that intersect, second below first" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 -5 8 ), ( "1", vec2 -5 4 ), ( "2", vec2 -5 5 ), ( "3", vec2 -5 0 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Just (vec2 -5 4.5)
        , test "that do NOT intersect, first below second" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 -5 5 ), ( "1", vec2 -5 0 ), ( "2", vec2 -5 8 ), ( "3", vec2 -5 6 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Nothing
        , test "that do NOT intersect, second below first" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 -5 8 ), ( "1", vec2 -5 6 ), ( "2", vec2 -5 5 ), ( "3", vec2 -5 0 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Nothing
        ]


intersectParallelSegmentsTests : Test
intersectParallelSegmentsTests =
    describe "intersect with non-intersecting parallel segments"
        [ test "that are vertical" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 5 0 ), ( "1", vec2 5 10 ), ( "2", vec2 0 0 ), ( "3", vec2 0 10 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Nothing
        , test "that are horizontal" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 0 0 ), ( "1", vec2 5 0 ), ( "2", vec2 0 -5 ), ( "3", vec2 6 -5 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Nothing
        , test "that are slanted up-right" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 0 0 ), ( "1", vec2 7 7 ), ( "2", vec2 1 0 ), ( "3", vec2 8 7 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Nothing
        , test "that are slanted down-right" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 0 7 ), ( "1", vec2 7 0 ), ( "2", vec2 1 8 ), ( "3", vec2 8 1 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Nothing
        ]


intersectSlantedCollinearSegmentsTests : Test
intersectSlantedCollinearSegmentsTests =
    describe "intersect with slanted, collinear segments"
        [ test "that intersect" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 1 1 ), ( "1", vec2 10 10 ), ( "2", vec2 5 5 ), ( "3", vec2 15 15 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Just (vec2 7.5 7.5)
        , test "that do NOT intersect, second above-right of first" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 1 1 ), ( "1", vec2 10 10 ), ( "2", vec2 10.1 10.1 ), ( "3", vec2 15 15 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Nothing
        , test "that do NOT intersect, first above-right of second" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 10.1 10.1 ), ( "1", vec2 15 15 ), ( "2", vec2 1 1 ), ( "3", vec2 10 10 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Nothing
        ]


intersectSlantedSegmentsTests : Test
intersectSlantedSegmentsTests =
    describe "intersect with slanted segments"
        [ test "that intersect" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 1 1 ), ( "1", vec2 10 10 ), ( "2", vec2 1 10 ), ( "3", vec2 10 1 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Just (vec2 5.5 5.5)
        , test "that intersect (2)" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 0 0 ), ( "1", vec2 5 10 ), ( "2", vec2 0 5 ), ( "3", vec2 5 5 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Just (vec2 2.5 5)
        , test "that do NOT intersect" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 1 1 ), ( "1", vec2 10 10 ), ( "2", vec2 2 1 ), ( "3", vec2 11 10 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Nothing
        , test "that do NOT intersect (2)" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 0 0 ), ( "1", vec2 5 10 ), ( "2", vec2 0.1 0 ), ( "3", vec2 5.1 9.9 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Nothing
        ]


intersectHorizontalCollinearSegmentsTests : Test
intersectHorizontalCollinearSegmentsTests =
    describe "intersect with horizontal, collinear segments"
        [ test "that DO intersect, first left of second" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 -5 -5 ), ( "1", vec2 0 -5 ), ( "2", vec2 -2 -5 ), ( "3", vec2 3 -5 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Just (vec2 -1 -5)
        , test "that DO intersect, second left of first" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 0 0 ), ( "1", vec2 5 0 ), ( "2", vec2 -2 0 ), ( "3", vec2 1 0 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Just (vec2 0.5 0)
        , test "that DO intersect, first contained within second" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 1 0 ), ( "1", vec2 2 0 ), ( "2", vec2 0 0 ), ( "3", vec2 10 0 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Just (vec2 1.5 0)
        , test "that DO intersect, second contained within first" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 0 0 ), ( "1", vec2 10 0 ), ( "2", vec2 1 0 ), ( "3", vec2 2 0 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Just (vec2 1.5 0)
        , test "that DO NOT intersect, first left of second" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 -5 -5 ), ( "1", vec2 0 -5 ), ( "2", vec2 2 -5 ), ( "3", vec2 5 -5 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Nothing
        , test "that DO NOT intersect, first right of second" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 2 -5 ), ( "1", vec2 5 -5 ), ( "2", vec2 -5 -5 ), ( "3", vec2 0 -5 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Nothing
        ]


intersectHorizontalSegmentsTests : Test
intersectHorizontalSegmentsTests =
    describe "intersect horizontal lines"
        [ test "that DO NOT intersect" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 10 1 ), ( "1", vec2 20 1 ), ( "2", vec2 10 55 ), ( "3", vec2 20 55 ) ]

                    edge1 =
                        Edge "0" "1"

                    edge2 =
                        Edge "2" "3"
                in
                    Expect.equal (intersect points edge1 edge2) (Nothing)
        , test "that DO intersect, second after first" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 10 1 ), ( "1", vec2 20 1 ), ( "2", vec2 19 1 ), ( "3", vec2 30 1 ) ]

                    edge1 =
                        Edge "0" "1"

                    edge2 =
                        Edge "2" "3"
                in
                    Expect.equal (intersect points edge1 edge2) <| Just (vec2 19.5 1)
        , test "that DO intersect, first after second" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 10 1 ), ( "1", vec2 20 1 ), ( "2", vec2 8 1 ), ( "3", vec2 12 1 ) ]

                    edge1 =
                        Edge "0" "1"

                    edge2 =
                        Edge "2" "3"
                in
                    Expect.equal (intersect points edge1 edge2) <| Just (vec2 11 1)
        , test "that DO intersect, second contained within first" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 0 0 ), ( "1", vec2 10 0 ), ( "2", vec2 1 0 ), ( "3", vec2 9 0 ) ]

                    edge1 =
                        Edge "0" "1"

                    edge2 =
                        Edge "2" "3"
                in
                    Expect.equal (intersect points edge1 edge2) <| Just (vec2 5 0)
        , test "that DO intersect, first contained within second" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 1 0 ), ( "1", vec2 2 0 ), ( "2", vec2 0 0 ), ( "3", vec2 5 0 ) ]

                    edge1 =
                        Edge "0" "1"

                    edge2 =
                        Edge "2" "3"
                in
                    Expect.equal (intersect points edge1 edge2) <| Just (vec2 1.5 0)
        ]


intersectVerticalSegmentsTests : Test
intersectVerticalSegmentsTests =
    describe "intersect vertical lines"
        [ test "that DO NOT intersect, first BELOW second" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 5 1 ), ( "1", vec2 5 10 ), ( "2", vec2 5.00001 10.1 ), ( "3", vec2 5.000011 20 ) ]

                    edge1 =
                        Edge "0" "1"

                    edge2 =
                        Edge "2" "3"
                in
                    Expect.equal (intersect points edge1 edge2) (Nothing)
        , test "that DO NOT intersect, first ABOVE second" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 5 1 ), ( "1", vec2 5 10 ), ( "2", vec2 5.00001 0 ), ( "3", vec2 5.000011 0.9 ) ]

                    edge1 =
                        Edge "0" "1"

                    edge2 =
                        Edge "2" "3"
                in
                    Expect.equal (intersect points edge1 edge2) (Nothing)
        , test "that DO intersect, second ABOVE first" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 0 -5 ), ( "1", vec2 0 3 ), ( "2", vec2 0 1 ), ( "3", vec2 0 5 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Just (vec2 0 2)
        , test "that DO intersect, first ABOVE second" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 0 -5 ), ( "1", vec2 0 3 ), ( "2", vec2 0 -8 ), ( "3", vec2 0 -3 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Just (vec2 0 -4)
        , test "that DO intersect, second contained within first" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 7 0 ), ( "1", vec2 7 10 ), ( "2", vec2 7 8 ), ( "3", vec2 7 4 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Just (vec2 7 6)
        , test "that DO intersect, first contained within second" <|
            \_ ->
                let
                    points =
                        Dict.fromList
                            [ ( "0", vec2 7 8 ), ( "1", vec2 7 4 ), ( "2", vec2 7 0 ), ( "3", vec2 7 10 ) ]
                in
                    Expect.equal (intersect points (Edge "0" "1") (Edge "2" "3")) <| Just (vec2 7 6)
        ]


nearlyEqualTests : Test
nearlyEqualTests =
    describe "nearlyEqual"
        [ test "identifies nearly-equal positive numbers" <|
            \_ ->
                Expect.equal (nearlyEqual 71.000000023 71.000000027) True
        , test "identifies nearly-equal negative numbers" <|
            \_ ->
                Expect.equal (nearlyEqual -37.00131325 -37.00131329) True
        , test "identifies NOT nearly-equal numbers" <|
            \_ ->
                Expect.equal (nearlyEqual 5.0 -5.0) False
        , test "identifies exactly-equal numbers" <|
            \_ ->
                Expect.equal (nearlyEqual 7 7) True
        ]
