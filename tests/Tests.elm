module Tests exposing (..)

import Math.Vector2 exposing (fromTuple, vec2)
import Test exposing (Test, describe, test, todo, skip, only)
import Expect
import String
import Edge exposing (allIntersections, bisect, intersect, nearlyEqual)
import Types exposing (Edge)


all : Test
all =
    describe "Edge module"
        [ bisectTests
        , intersectTests
        , nearlyEqualTests
        ]


bisectTests : Test
bisectTests =
    describe "bisect"
        [ test "with a horizontal line" <|
            \_ ->
                Expect.equal (bisect (vec2 0 0) (vec2 100 0)) (vec2 50 0)
        , test "with a vertical line" <|
            \_ ->
                Expect.equal (bisect (vec2 0 0) (vec2 0 100)) (vec2 0 50)
        , test "with an offset, diagonal line" <|
            \_ ->
                Expect.equal (bisect (vec2 10 10) (vec2 20 20)) (vec2 15 15)
        ]


intersectTests : Test
intersectTests =
    describe "intersect"
        [ intersectCollinearSegmentsTests
        , intersectSlantedSegmentsTests
        , skip intersectHorizontalSegmentsTests
        , skip intersectVerticalSegmentsTests
        ]


intersectCollinearSegmentsTests : Test
intersectCollinearSegmentsTests =
    describe "collinear lines"
        [ intersectHorizontalCollinearSegmentsTests
        , intersectVerticalCollinearSegmentsTests
        , intersectSlantedCollinearSegmentsTests
        ]


intersectVerticalCollinearSegmentsTests : Test
intersectVerticalCollinearSegmentsTests =
    describe "that are vertical"
        [ test "and DO intersect, first below second" <|
            \_ ->
                let
                    points =
                        [ (vec2 -5 5), (vec2 -5 0), (vec2 -5 8), (vec2 -5 4) ]
                in
                    Expect.equal (intersect points (Edge 0 1) (Edge 2 3)) <| Just (vec2 -5 4.5)
        ]


intersectSlantedCollinearSegmentsTests : Test
intersectSlantedCollinearSegmentsTests =
    describe "that are slanted"
        [ test "and DO intersect" <|
            \_ ->
                let
                    points =
                        [ (vec2 1 1), (vec2 10 10), (vec2 5 5), (vec2 15 15) ]
                in
                    Expect.equal (intersect points (Edge 0 1) (Edge 2 3)) <| Just (vec2 7.5 7.5)
        ]


intersectSlantedSegmentsTests : Test
intersectSlantedSegmentsTests =
    describe "that are slanted"
        [ test "and DO intersect" <|
            \_ ->
                let
                    points =
                        [ (vec2 1 1), (vec2 10 10), (vec2 1 10), (vec2 10 1) ]
                in
                    Expect.equal (intersect points (Edge 0 1) (Edge 2 3)) <| Just (vec2 5.5 5.5)
        , test "and do NOT intersect" <|
            \_ ->
                let
                    points =
                        [ (vec2 1 1), (vec2 10 10), (vec2 2 1), (vec2 11 10) ]
                in
                    Expect.equal (intersect points (Edge 0 1) (Edge 2 3)) <| Nothing
        ]


intersectHorizontalCollinearSegmentsTests : Test
intersectHorizontalCollinearSegmentsTests =
    describe "that are horizontal"
        [ test "that DO intersect, first left of second" <|
            \_ ->
                let
                    points =
                        [ (vec2 -5 -5), (vec2 0 -5), (vec2 -2 -5), (vec2 3 -5) ]
                in
                    Expect.equal (intersect points (Edge 0 1) (Edge 2 3)) <| Just (vec2 -1 -5)
        , test "that DO intersect, second left of first" <|
            \_ ->
                let
                    points =
                        [ (vec2 0 0), (vec2 5 0), (vec2 -2 0), (vec2 1 0) ]
                in
                    Expect.equal (intersect points (Edge 0 1) (Edge 2 3)) <| Just (vec2 0.5 0)
        , test "that DO intersect, first contained within second" <|
            \_ ->
                let
                    points =
                        [ (vec2 1 0), (vec2 2 0), (vec2 0 0), (vec2 10 0) ]
                in
                    Expect.equal (intersect points (Edge 0 1) (Edge 2 3)) <| Just (vec2 1.5 0)
        , test "that DO intersect, second contained within first" <|
            \_ ->
                let
                    points =
                        [ (vec2 0 0), (vec2 10 0), (vec2 1 0), (vec2 2 0) ]
                in
                    Expect.equal (intersect points (Edge 0 1) (Edge 2 3)) <| Just (vec2 1.5 0)
        , test "that DO NOT intersect, first left of second" <|
            \_ ->
                let
                    points =
                        [ (vec2 -5 -5), (vec2 0 -5), (vec2 2 -5), (vec2 5 -5) ]
                in
                    Expect.equal (intersect points (Edge 0 1) (Edge 2 3)) <| Nothing
        , test "that DO NOT intersect, first right of second" <|
            \_ ->
                let
                    points =
                        [ (vec2 2 -5), (vec2 5 -5), (vec2 -5 -5), (vec2 0 -5) ]
                in
                    Expect.equal (intersect points (Edge 0 1) (Edge 2 3)) <| Nothing
        ]


intersectArbitrarySegmentsTests : Test
intersectArbitrarySegmentsTests =
    describe "with two arbitrary lines"
        [ test "that DO intersect" <|
            \_ ->
                let
                    points =
                        [ (vec2 -5 -5), (vec2 0 -5), (vec2 -2 -5), (vec2 3 -5) ]
                in
                    Expect.equal (intersect points (Edge 0 1) (Edge 2 3)) <| Just (vec2 0 -4)
        ]


intersectHorizontalSegmentsTests : Test
intersectHorizontalSegmentsTests =
    describe "with two horizontal lines"
        [ test "that DO NOT intersect" <|
            \_ ->
                let
                    points =
                        [ (vec2 10 1), (vec2 20 1), (vec2 10 55), (vec2 20 55) ]

                    edge1 =
                        Edge 0 1

                    edge2 =
                        Edge 2 3
                in
                    Expect.equal (intersect points edge1 edge2) (Nothing)
        , test "that DO intersect, second after first" <|
            \_ ->
                let
                    points =
                        [ (vec2 10 1), (vec2 20 1), (vec2 19 1), (vec2 30 1) ]

                    edge1 =
                        Edge 0 1

                    edge2 =
                        Edge 2 3
                in
                    Expect.equal (intersect points edge1 edge2) <| Just (vec2 19.5 1)
        , test "that DO intersect, first after second" <|
            \_ ->
                let
                    points =
                        [ (vec2 10 1), (vec2 20 1), (vec2 8 1), (vec2 12 1) ]

                    edge1 =
                        Edge 0 1

                    edge2 =
                        Edge 2 3
                in
                    Expect.equal (intersect points edge1 edge2) <| Just (vec2 11 1)
        , test "that DO intersect, second contained within first" <|
            \_ ->
                let
                    points =
                        [ (vec2 0 0), (vec2 10 0), (vec2 1 0), (vec2 9 0) ]

                    edge1 =
                        Edge 0 1

                    edge2 =
                        Edge 2 3
                in
                    Expect.equal (intersect points edge1 edge2) <| Just (vec2 5 0)
        , test "that DO intersect, first contained within second" <|
            \_ ->
                let
                    points =
                        [ (vec2 1 0), (vec2 2 0), (vec2 0 0), (vec2 5 0) ]

                    edge1 =
                        Edge 0 1

                    edge2 =
                        Edge 2 3
                in
                    Expect.equal (intersect points edge1 edge2) <| Just (vec2 1.5 0)
        ]


intersectVerticalSegmentsTests : Test
intersectVerticalSegmentsTests =
    describe "with two vertical lines"
        [ test "that DO NOT intersect, first BELOW second" <|
            \_ ->
                let
                    points =
                        [ (vec2 5 1), (vec2 5 10), (vec2 5.00001 10.1), (vec2 5.000011 20) ]

                    edge1 =
                        Edge 0 1

                    edge2 =
                        Edge 2 3
                in
                    Expect.equal (intersect points edge1 edge2) (Nothing)
        , test "that DO NOT intersect, first ABOVE second" <|
            \_ ->
                let
                    points =
                        [ (vec2 5 1), (vec2 5 10), (vec2 5.00001 0), (vec2 5.000011 0.9) ]

                    edge1 =
                        Edge 0 1

                    edge2 =
                        Edge 2 3
                in
                    Expect.equal (intersect points edge1 edge2) (Nothing)
        , test "that DO intersect, second ABOVE first" <|
            \_ ->
                let
                    points =
                        [ (vec2 0 -5), (vec2 0 3), (vec2 0 1), (vec2 0 5) ]
                in
                    Expect.equal (intersect points (Edge 0 1) (Edge 2 3)) <| Just (vec2 0 2)
        , test "that DO intersect, first ABOVE second" <|
            \_ ->
                let
                    points =
                        [ (vec2 0 -5), (vec2 0 3), (vec2 0 -8), (vec2 0 -3) ]
                in
                    Expect.equal (intersect points (Edge 0 1) (Edge 2 3)) <| Just (vec2 0 -4)
        , test "that DO intersect, second contained within first" <|
            \_ ->
                let
                    points =
                        [ (vec2 7 0), (vec2 7 10), (vec2 7 8), (vec2 7 4) ]
                in
                    Expect.equal (intersect points (Edge 0 1) (Edge 2 3)) <| Just (vec2 7 6)
        , test "that DO intersect, first contained within second" <|
            \_ ->
                let
                    points =
                        [ (vec2 7 8), (vec2 7 4), (vec2 7 0), (vec2 7 10) ]
                in
                    Expect.equal (intersect points (Edge 0 1) (Edge 2 3)) <| Just (vec2 7 6)
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
        ]
