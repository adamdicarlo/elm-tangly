module View exposing (view)

import Array
import Color exposing (Color)
import Collage exposing (Form, collage, circle, defaultLine, move, outlined, segment, traced)
import Element exposing (toHtml)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Math.Vector2 exposing (toTuple, vec2)
import Constants exposing (pointRadius)
import Edge exposing (allIntersections)
import Types
    exposing
        ( Cursor(Bored, Dragging, Hovering)
        , Model
        , Msg(NextLevel)
        , Point
        , PointIndex
        )


fallbackPoint : Point
fallbackPoint =
    vec2 -9999 9999


view : Model -> Html Msg
view model =
    let
        width =
            toFloat model.width

        height =
            toFloat model.height

        intersections =
            allIntersections model.points model.edges

        canvas =
            List.concat
                [ viewEdges model
                , viewPoints model
                , viewIntersections model intersections
                ]
                |> collage model.width model.height
                |> toHtml
    in
        div [ class "tangly" ]
            [ canvas
            , viewHUD model intersections
            ]


highlightedColor : Color
highlightedColor =
    Color.green


normalColor : Color
normalColor =
    Color.black


styleForIndex : Cursor -> PointIndex -> Color
styleForIndex cursor index =
    let
        highlightIndex cursor =
            case cursor of
                Bored ->
                    Nothing

                Hovering index ->
                    Just index

                Dragging index ->
                    Just index
    in
        case highlightIndex cursor of
            Just highlightPointIndex ->
                if highlightPointIndex == index then
                    highlightedColor
                else
                    normalColor

            _ ->
                normalColor


viewPoints : Model -> List Form
viewPoints model =
    let
        viewPoint index point =
            circle pointRadius
                |> Collage.filled (styleForIndex model.cursor index)
                |> move (toTuple point)
    in
        List.indexedMap viewPoint model.points


viewEdges : Model -> List Form
viewEdges model =
    let
        pointArray =
            Array.fromList model.points

        pointAt index =
            Array.get index pointArray
                |> Maybe.withDefault fallbackPoint
                |> toTuple

        viewEdge { from, to } =
            segment (pointAt from) (pointAt to) |> traced { defaultLine | width = 2 }
    in
        List.map viewEdge model.edges


viewIntersections : Model -> List Point -> List Form
viewIntersections model intersections =
    let
        viewIntersection point =
            circle 4 |> outlined { defaultLine | color = Color.red } |> move (toTuple point)
    in
        List.map viewIntersection intersections


pluralize : String -> String -> number -> String
pluralize singular plural count =
    if count == 1 then
        singular
    else
        plural


viewHUD : Model -> List Point -> Html Msg
viewHUD model intersections =
    let
        count =
            List.length intersections

        level =
            div [ class "level" ] [ text <| "Level " ++ (toString model.levelNumber) ]

        progress =
            div [ class "progress" ]
                [ text <|
                    toString count
                        ++ " "
                        ++ (pluralize "intersection" "intersections" count)
                ]

        solved =
            if model.levelSolved then
                [ div [ class "solved" ]
                    [ text "Solved!"
                    , button [ class "btn-3d red", onClick NextLevel ] [ text "Next level →" ]
                    ]
                ]
            else
                []
    in
        div [ class "hud" ]
            (div [ class "status-container" ] [ level, progress ] :: solved)
