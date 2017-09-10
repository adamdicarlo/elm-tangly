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
    , levelCodeModalActive : Bool
    , levelNumber : Int
    , levelSolved : Bool
    , mode : Mode
    }


type alias Point =
    Vec2


type alias PointIndex =
    Int


type Cursor
    = Bored
    | Hovering PointIndex
    | Dragging PointIndex


type Mode
    = Edit
    | Play


type Msg
    = EditMode
    | MouseDown Mouse.Position
    | MouseMove Mouse.Position
    | MouseUp Mouse.Position
    | NextLevel
    | NoOp
    | PlayMode
    | ToggleLevelCodeModal
    | WindowSize Window.Size
