module Types exposing (..)

import Dict exposing (Dict)
import Keyboard
import Math.Vector2 exposing (Vec2)
import Mouse
import Window


type alias Point =
    Vec2


type alias EdgeId =
    String


type alias PointId =
    String


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
    , additiveSelection : Bool
    , cursor : Cursor
    , edges : EdgeDict
    , levelCodeModalActive : Bool
    , levelNumber : Int
    , levelSolved : Bool
    , mode : Mode
    , points : PointDict
    , selectedEdges : Dict EdgeId ()
    , selectedPoints : Dict PointId ()
    }


type Cursor
    = Bored
    | Hovering PointId
    | Dragging PointId


type Mode
    = Edit
    | Play


type Msg
    = CreateEdge PointId PointId
    | Delete
    | EditMode
    | KeyDown Keyboard.KeyCode
    | KeyUp Keyboard.KeyCode
    | MouseDown Mouse.Position
    | MouseMove Mouse.Position
    | MouseUp Mouse.Position
    | NextLevel
    | NoOp
    | PlayMode
    | ToggleLevelCodeModal
    | WindowSize Window.Size
