module Types exposing (Edge, EdgeDict, EdgeId, Point, PointDict, PointId)

import Dict exposing (Dict)
import Math.Vector2 exposing (Vec2)


type alias Point =
    Vec2


type alias EdgeId =
    String


type alias PointId =
    String


type alias EdgeDict =
    Dict EdgeId Edge


type alias PointDict =
    Dict PointId Vec2


type alias Edge =
    { from : PointId
    , to : PointId
    }
