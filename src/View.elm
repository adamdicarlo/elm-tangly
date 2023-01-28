module View exposing (view)

import Constants exposing (pointRadius)
import Dict
import Edge exposing (allIntersections)
import GraphicSVG as Svg exposing (Color, LineType, Shape)
import GraphicSVG.Widget as Widget
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Math.Vector2 as Vector2 exposing (vec2)
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
    ( Vector2.getX p, Vector2.getY p )


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
                |> Widget.icon "gfx" width height

        canvasStyleAttrs : List (Html.Attribute msg)
        canvasStyleAttrs =
            case model.cursor of
                Bored ->
                    if model.mode == Edit && isSelectionEmpty model then
                        -- indicate that clicking will create a vertex
                        [ Attributes.style "cursor" "cell" ]

                    else
                        []

                Hovering _ ->
                    [ Attributes.style "cursor" "pointer" ]

                Dragging _ ->
                    [ Attributes.style "cursor" "move" ]
    in
    Html.div (Attributes.class "tangly" :: canvasStyleAttrs)
        [ canvas
        , viewHUD model intersections
        , viewLevelCodeModal model
        ]


hoverDragColor : Color
hoverDragColor =
    Svg.green


normalColor : Color
normalColor =
    Svg.black


selectedColor : Color
selectedColor =
    Svg.red


selectionLineType : LineType
selectionLineType =
    Svg.solid 2


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
        points : List ( PointId, Vector2.Vec2 )
        points =
            Dict.toList model.points

        viewPoint : ( PointId, Point ) -> Shape Never
        viewPoint ( id, point ) =
            Svg.circle pointRadius
                |> Svg.filled (styleForPointId model id)
                |> Svg.move (toTuple point)

        viewSelection : ( PointId, Point ) -> Maybe (Shape Never)
        viewSelection ( id, point ) =
            if Set.member id model.selectedPoints then
                Svg.rect (2 * pointRadius + 4) (2 * pointRadius + 4)
                    |> Svg.outlined selectionLineType selectedColor
                    |> Svg.move (toTuple point)
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
            Svg.line (pointById from) (pointById to)
                |> Svg.outlined (Svg.solid 2) Svg.purple
    in
    Dict.values model.edges |> List.map viewEdge


viewIntersections : Model -> List Point -> List (Shape Never)
viewIntersections _ intersections =
    let
        viewIntersection : Point -> Shape Never
        viewIntersection point =
            Svg.circle 4 |> Svg.outlined (Svg.solid 1) Svg.red |> Svg.move (toTuple point)
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
            Html.div [ Attributes.class "level" ]
                [ String.concat
                    [ "Level "
                    , String.fromInt model.levelNumber
                    ]
                    |> Html.text
                ]

        progress : Html Msg
        progress =
            Html.div [ Attributes.class "progress" ]
                [ pluralize "intersection" "intersections" count
                    |> Html.text
                ]

        status : Html Msg
        status =
            Html.div [ Attributes.class "statusContainer" ]
                [ level
                , progress
                ]
    in
    Html.div
        [ Attributes.classList
            [ ( "hud", True )
            , ( "editMode", model.mode == Edit )
            ]
        ]
        [ status
        , viewHUDActions model
        ]


viewHUDActions : Model -> Html Msg
viewHUDActions model =
    Html.div [ Attributes.class "action" ]
        (case model.mode of
            Play ->
                if model.levelSolved then
                    [ iconLabel "🎊" "Solved!"
                    , nextLevelButton
                    ]

                else
                    [ editButton ]

            Edit ->
                List.append [ levelCodeButton, playButton ] <|
                    if isSelectionEmpty model then
                        []

                    else
                        [ deleteButton, createEdgeButton model ]
        )


iconLabel : String -> String -> Html Msg
iconLabel icon label =
    Html.span []
        [ Html.span [ Attributes.class "icon" ] [ Html.text icon ]
        , Html.span [] [ Html.text label ]
        ]


createEdgeButton : Model -> Html Msg
createEdgeButton model =
    case Set.toList model.selectedPoints of
        -- Exactly two vertices should be selected
        from :: to :: [] ->
            if edgeExists model from to then
                Html.button
                    [ Attributes.class "small gray disabled btn-3d"
                    , Attributes.disabled True
                    ]
                    [ iconLabel "🌟" "Create Edge" ]

            else
                Html.button
                    [ Attributes.class "small green btn-3d"
                    , Events.onClick <| CreateEdge from to
                    ]
                    [ iconLabel "🌟" "Create Edge" ]

        _ ->
            Html.span [] []


editButton : Html Msg
editButton =
    Html.button
        [ Attributes.class "small blue btn-3d"
        , Events.onClick EditMode
        ]
        [ iconLabel "🔧" "Edit" ]


levelCodeButton : Html Msg
levelCodeButton =
    Html.button
        [ Attributes.class "small green btn-3d"
        , Events.onClick ToggleLevelCodeModal
        ]
        [ iconLabel "👾" "Code" ]


nextLevelButton : Html Msg
nextLevelButton =
    Html.button
        [ Attributes.class "btn-3d red"
        , Events.onClick NextLevel
        ]
        [ Html.text "Next level →" ]


deleteButton : Html Msg
deleteButton =
    Html.button
        [ Attributes.class "small red btn-3d"
        , Events.onClick Delete
        ]
        [ iconLabel "💀" "Delete" ]


playButton : Html Msg
playButton =
    Html.button
        [ Attributes.class "small blue btn-3d"
        , Events.onClick PlayMode
        ]
        [ iconLabel "🎮" "Play" ]


viewLevelCodeModal : Model -> Html Msg
viewLevelCodeModal model =
    if model.levelCodeModalActive then
        Html.div [ Attributes.class "levelCodeModalBackdrop open" ]
            [ Html.div [ Attributes.class "levelCodeModal" ]
                [ Html.pre []
                    [ Html.text <| levelToCode model
                    ]
                , Html.button
                    [ Attributes.class "btn-3d green"
                    , Events.onClick ToggleLevelCodeModal
                    ]
                    [ Html.text "OK" ]
                ]
            ]

    else
        Html.div [ Attributes.class "levelCodeModalBackdrop" ] []


format3 : String -> String -> String -> String -> String
format3 format a b c =
    case format |> String.split "%" of
        w :: x :: y :: [ z ] ->
            [ w, a, x, b, y, c, z ] |> String.concat

        _ ->
            [ "Bug in format passed to format3:"
            , format
            ]
                |> String.concat


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


levelToCode : Model -> String
levelToCode model =
    let
        varName : String
        varName =
            "level" ++ String.fromInt model.levelNumber

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
