module Types exposing (..)

import Dict exposing (Dict)
import Math.Vector2 exposing (Vec2)
import Mouse
import Window


type alias EdgeDict =
    Dict EdgeId Edge


type alias PointDict =
    Dict PointId Point


type alias Edge =
    { from : PointId
    , to : PointId
    }


type alias Model =
    { width : Int
    , height : Int
    , edges : EdgeDict
    , points : PointDict
    , cursor : Cursor
    , levelCodeModalActive : Bool
    , levelNumber : Int
    , levelSolved : Bool
    , mode : Mode
    , selectedEdges : Dict EdgeId ()
    , selectedPoints : Dict PointId ()
    }


type alias Point =
    Vec2


type alias EdgeId =
    Int


type alias PointId =
    Int


type Cursor
    = Bored
    | Hovering PointId
    | Dragging PointId


type Mode
    = Edit
    | Play


type Msg
    = Delete
    | EditMode
    | MouseDown Mouse.Position
    | MouseMove Mouse.Position
    | MouseUp Mouse.Position
    | NextLevel
    | NoOp
    | PlayMode
    | ToggleLevelCodeModal
    | WindowSize Window.Size
