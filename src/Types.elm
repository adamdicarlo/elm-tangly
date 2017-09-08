module Types exposing (..)

import Math.Vector2 exposing (Vec2)
import Mouse
import Window


type alias Edge =
    { from : PointIndex
    , to : PointIndex
    }


type alias Model =
    { width : Int
    , height : Int
    , points : List Point
    , edges : List Edge
    , cursor : Cursor
    }


type alias Point =
    Vec2


type alias PointIndex =
    Int


type Cursor
    = Bored
    | Hovering PointIndex
    | Dragging PointIndex


type Msg
    = MouseDown Mouse.Position
    | MouseMove Mouse.Position
    | MouseUp Mouse.Position
    | NoOp
    | WindowSize Window.Size
