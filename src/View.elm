module View exposing (view)

import Color exposing (Color)
import Dict exposing (Dict, isEmpty)
import Collage exposing (Form, collage, circle, defaultLine, move, outlined, rect, segment, traced)
import Element exposing (toHtml)
import Html exposing (Html, button, code, div, pre, span, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Math.Vector2 exposing (toTuple, vec2)
import Constants exposing (pointRadius)
import Edge exposing (allIntersections)
import Types
    exposing
        ( Cursor(Bored, Dragging, Hovering)
        , Edge
        , EdgeId
        , Mode(Edit, Play)
        , Model
        , Msg(Delete, EditMode, NextLevel, PlayMode, ToggleLevelCodeModal)
        , Point
        , PointId
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
            , viewLevelCodeModal model
            ]


hoverDragColor : Color
hoverDragColor =
    Color.green


normalColor : Color
normalColor =
    Color.black


selectedColor : Color
selectedColor =
    Color.red


styleForPointId : Model -> PointId -> Color
styleForPointId model id =
    let
        hoverDragId =
            case model.cursor of
                Bored ->
                    Nothing

                Hovering id ->
                    Just id

                Dragging id ->
                    Just id
    in
        case hoverDragId of
            -- Hovering or dragging takes highlight precedence
            Just hoverDragPointId ->
                if hoverDragPointId == id then
                    hoverDragColor
                else
                    normalColor

            _ ->
                normalColor


viewPoints : Model -> List Form
viewPoints model =
    let
        points =
            Dict.toList model.points

        viewPoint ( id, point ) =
            circle pointRadius
                |> Collage.filled (styleForPointId model id)
                |> move (toTuple point)

        viewSelection : ( PointId, Point ) -> Maybe Form
        viewSelection ( id, point ) =
            case Dict.get id model.selectedPoints of
                Just _ ->
                    rect (2 * pointRadius + 4) (2 * pointRadius + 4)
                        |> outlined defaultLine
                        |> move (toTuple point)
                        |> Just

                Nothing ->
                    Nothing
    in
        List.map viewPoint points
            ++ List.filterMap viewSelection points


viewEdges : Model -> List Form
viewEdges model =
    let
        pointById id =
            Dict.get id model.points
                |> Maybe.withDefault fallbackPoint
                |> toTuple

        viewEdge { from, to } =
            segment (pointById from) (pointById to)
                |> traced { defaultLine | width = 2 }
    in
        Dict.values model.edges |> List.map viewEdge


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

        status =
            div [ class "statusContainer" ] [ level, progress ]

        actions =
            div [ class "action" ]
                (case model.mode of
                    Play ->
                        if model.levelSolved then
                            [ iconLabel "🎊" "Solved!"
                            , nextLevelButton
                            ]
                        else
                            [ editButton ]

                    Edit ->
                        (if isEmpty model.selectedEdges && isEmpty model.selectedPoints then
                            []
                         else
                            [ deleteButton ]
                        )
                            ++ [ playButton, levelCodeButton ]
                )
    in
        div [ class "hud" ] [ status, actions ]


iconLabel : String -> String -> Html Msg
iconLabel icon label =
    span [] [ span [ class "icon" ] [ text icon ], span [] [ text label ] ]


editButton : Html Msg
editButton =
    button [ class "small red btn-3d", onClick EditMode ] [ iconLabel "🔧" "Edit" ]


levelCodeButton : Html Msg
levelCodeButton =
    button [ class "small green btn-3d", onClick ToggleLevelCodeModal ] [ iconLabel "👾" "Code" ]


nextLevelButton : Html Msg
nextLevelButton =
    button [ class "btn-3d red", onClick NextLevel ] [ text "Next level →" ]


deleteButton : Html Msg
deleteButton =
    button [ class "small red btn-3d", onClick Delete ] [ iconLabel "💀" "Delete" ]


playButton : Html Msg
playButton =
    button [ class "small red btn-3d", onClick PlayMode ] [ iconLabel "🎮" "Play" ]


viewLevelCodeModal : Model -> Html Msg
viewLevelCodeModal model =
    if model.levelCodeModalActive then
        div [ class "levelCodeModalBackdrop" ]
            [ div [ class "levelCodeModal" ]
                [ pre []
                    [ text <| levelToCode model
                    ]
                , button [ class "btn-3d green", onClick ToggleLevelCodeModal ] [ text "OK" ]
                ]
            ]
    else
        div [] []


levelToCode : Model -> String
levelToCode model =
    let
        varName =
            "level" ++ (toString model.levelNumber)

        showEdge : ( EdgeId, Edge ) -> String
        showEdge ( id, { from, to } ) =
            "( " ++ toString id ++ ", Edge " ++ (toString from) ++ " " ++ (toString to) ++ " )"

        showPoint : ( PointId, Point ) -> String
        showPoint ( id, p ) =
            let
                ( x, y ) =
                    toTuple p
            in
                "( " ++ toString id ++ ", vec2 " ++ (toString x) ++ " " ++ (toString y) ++ " )"

        joinCodeLines a b =
            a
                ++ "\n            "
                ++ (if String.isEmpty b then
                        ""
                    else
                        ", "
                   )
                ++ b

        points =
            Dict.toList model.points
                |> List.map showPoint
                |> List.foldl joinCodeLines ""

        edges =
            Dict.toList model.edges
                |> List.map showEdge
                |> List.foldl joinCodeLines ""
    in
        varName
            ++ " : Level\n"
            ++ varName
            ++ " =\n"
            ++ "    { edges =\n"
            ++ "        Dict.fromList\n"
            ++ "            [ "
            ++ edges
            ++ "]\n"
            ++ "    , points =\n"
            ++ "        Dict.fromList\n"
            ++ "            [ "
            ++ points
            ++ "]\n"
            ++ "    }\n"
