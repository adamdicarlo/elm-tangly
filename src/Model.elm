module Model exposing (..)

import Dict exposing (Dict)
import List.Extra
import Math.Vector2 exposing (distance, fromTuple)
import Constants exposing (pointRadius)
import Types
    exposing
        ( Model
        , PointId
        , Point
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


screenToPoint : Model -> Int -> Int -> Point
screenToPoint model x y =
    let
        xOrigin =
            (toFloat model.width) / 2.0

        yOrigin =
            (toFloat model.height) / 2.0
    in
        fromTuple ( (toFloat x) - xOrigin, yOrigin - (toFloat y) )
