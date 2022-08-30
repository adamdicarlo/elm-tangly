module Model exposing (..)

import Constants exposing (pointRadius)
import Dict exposing (Dict)
import List.Extra as List
import Math.Vector2 exposing (distanceSquared, vec2)
import Types
    exposing
        ( Model
        , Point
        , PointId
        )


findPointNear : Dict PointId Point -> Point -> Maybe PointId
findPointNear points test =
    let
        distanceWithId : ( PointId, Point ) -> ( PointId, Float )
        distanceWithId ( id, point ) =
            ( id, distanceSquared point test )

        ifCloseEnough ( id, dist2 ) =
            if sqrt dist2 < pointRadius then
                Just id

            else
                Nothing
    in
    -- Determine distance between `test` and each point and keep smallest
    points
        |> Dict.toList
        |> List.map distanceWithId
        |> List.minimumBy Tuple.second
        |> Maybe.andThen ifCloseEnough


isSelectionEmpty : Model -> Bool
isSelectionEmpty model =
    Dict.isEmpty model.selectedEdges && Dict.isEmpty model.selectedPoints


edgeExists : Model -> PointId -> PointId -> Bool
edgeExists model p1 p2 =
    Dict.values model.edges
        |> List.any
            (\{ from, to } ->
                (from == p1 && to == p2)
                    || (from == p2 && to == p1)
            )


screenToPoint : Model -> Float -> Float -> Point
screenToPoint model x y =
    let
        xOrigin =
            model.width / 2.0

        yOrigin =
            model.height / 2.0
    in
    vec2 (x - xOrigin) (yOrigin - y)
