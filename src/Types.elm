module Types exposing (..)

import Math.Vector2 exposing (Vec2)


type alias Point =
    Vec2


type alias Edge =
    { from : PointIndex
    , to : PointIndex
    }


type alias PointIndex =
    Int


type Cursor
    = Bored
    | Hovering PointIndex
    | Dragging PointIndex
