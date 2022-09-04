module View exposing (view)

import Constants exposing (pointRadius)
import Dict
import Edge exposing (allIntersections)
import GraphicSVG
    exposing
        ( Color
        , LineType
        , Shape
        , circle
        , line
        , move
        , outlined
        , rect
        , solid
        )
import GraphicSVG.Widget
import Html exposing (Html, button, div, pre, span, text)
import Html.Attributes exposing (class, disabled, style)
import Html.Events exposing (onClick)
import Math.Vector2 exposing (vec2)
import Model exposing (edgeExists, isSelectionEmpty)
import Set
import Types
    exposing
        ( Cursor(..)
        , Edge
        , EdgeId
        , Mode(..)
        , Model
        , Msg(..)
        , Point
        , PointId
        )


toTuple : Point -> ( Float, Float )
toTuple p =
    ( Math.Vector2.getX p, Math.Vector2.getY p )


fallbackPoint : Point
fallbackPoint =
    vec2 -9999 9999


view : Model -> Html Msg
view ({ edges, points, width, height } as model) =
    let
        intersections : List Point
        intersections =
            allIntersections points edges

        canvas : Html a
        canvas =
            List.concat
                [ viewEdges model
                , viewPoints model
                , viewIntersections model intersections
                ]
                |> GraphicSVG.Widget.icon "gfx" width height

        canvasStyleAttrs : List (Html.Attribute msg)
        canvasStyleAttrs =
            case model.cursor of
                Bored ->
                    if model.mode == Edit && isSelectionEmpty model then
                        -- indicate that clicking will create a vertex
                        [ style "cursor" "cell" ]

                    else
                        []

                Hovering _ ->
                    [ style "cursor" "pointer" ]

                Dragging _ ->
                    [ style "cursor" "move" ]
    in
    div (class "tangly" :: canvasStyleAttrs)
        [ canvas
        , viewHUD model intersections
        , viewLevelCodeModal model
        ]


hoverDragColor : Color
hoverDragColor =
    GraphicSVG.green


normalColor : Color
normalColor =
    GraphicSVG.black


selectedColor : Color
selectedColor =
    GraphicSVG.red


selectionLineType : LineType
selectionLineType =
    solid 2


styleForPointId : Model -> PointId -> Color
styleForPointId model id =
    let
        chooseColor : PointId -> Color
        chooseColor hoverDragPointId =
            -- Hovering or dragging takes highlight precedence
            if hoverDragPointId == id then
                hoverDragColor

            else
                normalColor
    in
    case model.cursor of
        Bored ->
            normalColor

        Hovering id_ ->
            chooseColor id_

        Dragging id_ ->
            chooseColor id_


viewPoints : Model -> List (Shape Never)
viewPoints model =
    let
        points : List ( PointId, Math.Vector2.Vec2 )
        points =
            Dict.toList model.points

        viewPoint : ( PointId, Point ) -> Shape userMsg
        viewPoint ( id, point ) =
            circle pointRadius
                |> GraphicSVG.filled (styleForPointId model id)
                |> move (toTuple point)

        viewSelection : ( PointId, Point ) -> Maybe (Shape Never)
        viewSelection ( id, point ) =
            if Set.member id model.selectedPoints then
                rect (2 * pointRadius + 4) (2 * pointRadius + 4)
                    |> outlined selectionLineType selectedColor
                    |> move (toTuple point)
                    |> Just

            else
                Nothing
    in
    List.concat
        [ List.map viewPoint points
        , List.filterMap viewSelection points
        ]


viewEdges : Model -> List (Shape Never)
viewEdges model =
    let
        pointById : PointId -> ( Float, Float )
        pointById id =
            Dict.get id model.points
                |> Maybe.withDefault fallbackPoint
                |> toTuple

        viewEdge : { from : PointId, to : PointId } -> Shape Never
        viewEdge { from, to } =
            line (pointById from) (pointById to)
                |> outlined (solid 2) GraphicSVG.purple
    in
    Dict.values model.edges |> List.map viewEdge


viewIntersections : Model -> List Point -> List (Shape Never)
viewIntersections _ intersections =
    let
        viewIntersection : Point -> Shape Never
        viewIntersection point =
            circle 4 |> outlined (solid 1) GraphicSVG.red |> move (toTuple point)
    in
    List.map viewIntersection intersections


pluralize : String -> String -> Int -> String
pluralize singular plural count =
    String.join " "
        [ String.fromInt count
        , if count == 1 then
            singular

          else
            plural
        ]


viewHUD : Model -> List Point -> Html Msg
viewHUD model intersections =
    let
        count : Int
        count =
            List.length intersections

        level : Html Msg
        level =
            div [ class "level" ]
                [ String.concat
                    [ "Level "
                    , String.fromInt model.levelNumber
                    ]
                    |> text
                ]

        progress : Html Msg
        progress =
            div [ class "progress" ]
                [ pluralize "intersection" "intersections" count
                    |> text
                ]

        status : Html Msg
        status =
            div [ class "statusContainer" ] [ level, progress ]

        className : String
        className =
            if model.mode == Edit then
                "hud editMode"

            else
                "hud"
    in
    div [ class className ] [ status, viewHUDActions model ]


viewHUDActions : Model -> Html Msg
viewHUDActions model =
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
                List.concat
                    [ if isSelectionEmpty model then
                        []

                      else
                        [ deleteButton, createEdgeButton model ]
                    , [ levelCodeButton, playButton ]
                    ]
        )


iconLabel : String -> String -> Html Msg
iconLabel icon label =
    span [] [ span [ class "icon" ] [ text icon ], span [] [ text label ] ]


createEdgeButton : Model -> Html Msg
createEdgeButton model =
    case Set.toList model.selectedPoints of
        -- Exactly two vertices should be selected
        from :: to :: [] ->
            if edgeExists model from to then
                button [ class "small gray disabled btn-3d", disabled True ] [ iconLabel "🌟" "Create Edge" ]

            else
                button [ class "small green btn-3d", onClick <| CreateEdge from to ] [ iconLabel "🌟" "Create Edge" ]

        _ ->
            span [] []


editButton : Html Msg
editButton =
    button [ class "small blue btn-3d", onClick EditMode ] [ iconLabel "🔧" "Edit" ]


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
    button [ class "small blue btn-3d", onClick PlayMode ] [ iconLabel "🎮" "Play" ]


viewLevelCodeModal : Model -> Html Msg
viewLevelCodeModal model =
    if model.levelCodeModalActive then
        div [ class "levelCodeModalBackdrop open" ]
            [ div [ class "levelCodeModal" ]
                [ pre []
                    [ text <| levelToCode model
                    ]
                , button [ class "btn-3d green", onClick ToggleLevelCodeModal ] [ text "OK" ]
                ]
            ]

    else
        div [ class "levelCodeModalBackdrop" ] []


format3 : String -> String -> String -> String -> String
format3 format a b c =
    case format |> String.split "%" of
        w :: x :: y :: [ z ] ->
            [ w, a, x, b, y, c, z ] |> String.concat

        _ ->
            [ "Bug in format passed to format3:", format ] |> String.concat


levelToCode : Model -> String
levelToCode model =
    let
        varName : String
        varName =
            "level" ++ String.fromInt model.levelNumber

        showEdge : ( EdgeId, Edge ) -> String
        showEdge ( id, { from, to } ) =
            format3
                "( \"%\", Edge % % )"
                id
                from
                to

        showPoint : ( PointId, Point ) -> String
        showPoint ( id, p ) =
            let
                ( x, y ) =
                    toTuple p
            in
            format3
                "( \"%\", vec2 % % )"
                id
                (String.fromFloat x)
                (String.fromFloat y)

        joinCodeLines : String -> String -> String
        joinCodeLines a b =
            [ a
            , if String.isEmpty b then
                ""

              else
                "\n            , "
            , b
            ]
                |> String.concat

        points : String
        points =
            Dict.toList model.points
                |> List.map showPoint
                |> List.foldl joinCodeLines ""

        edges : String
        edges =
            Dict.toList model.edges
                |> List.map showEdge
                |> List.foldl joinCodeLines ""
    in
    [ varName ++ " : Level"
    , varName ++ " ="
    , "    { edges ="
    , "        Dict.fromList"
    , "            [ " ++ edges
    , "            ]"
    , "    , points ="
    , "        Dict.fromList"
    , "            [ " ++ points
    , "            ]"
    , "    }"
    ]
        |> String.join "\n"
