module Model exposing (..)

import Constants exposing (pointRadius)
import Dict exposing (Dict)
import List.Extra
import Math.Vector2 exposing (distance, vec2)
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
            ( id, distance point test )

        closest =
            -- Determine distance between `test` and each point and keep smallest
            points
                |> Dict.toList
                |> List.map distanceWithId
                |> List.Extra.minimumBy Tuple.second
    in
    case closest of
        Nothing ->
            Nothing |> Debug.log "Bug! findPointNear received empty list?"

        Just ( closestId, closestDistance ) ->
            if closestDistance < pointRadius then
                Just closestId

            else
                Nothing


isSelectionEmpty : Model -> Bool
isSelectionEmpty model =
    Dict.isEmpty model.selectedEdges && Dict.isEmpty model.selectedPoints


screenToPoint : Model -> Float -> Float -> Point
screenToPoint model x y =
    let
        xOrigin =
            model.width / 2.0

        yOrigin =
            model.height / 2.0
    in
    vec2 (x - xOrigin) (yOrigin - y)
