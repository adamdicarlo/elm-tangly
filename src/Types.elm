module Types exposing (..)

import Dict exposing (Dict)
import Keyboard exposing (Key(..))
import Math.Vector2 exposing (Vec2)
import Set exposing (Set)


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


type alias Model =
    { height : Float
    , width : Float
    , additiveSelection : Bool
    , cursor : Cursor
    , edges : EdgeDict
    , levelCodeModalActive : Bool
    , levelNumber : Int
    , levelSolved : Bool
    , mode : Mode
    , points : PointDict
    , selectedEdges : Set EdgeId
    , selectedPoints : Set PointId
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
    | KeyDown Key
    | KeyUp Key
    | MouseDown Vec2
    | MouseMove Vec2
    | MouseUp Vec2
    | NextLevel
    | NoOp
    | PlayMode
    | ToggleLevelCodeModal
    | WindowSize Vec2
