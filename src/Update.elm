module Update exposing (init, update)

import Dict exposing (Dict)
import Task exposing (perform)
import Keyboard.Key
import Window
import Constants exposing (edgesForLevel, pointsForLevel)
import Edge exposing (allIntersections)
import Model exposing (findPointNear, screenToPoint)
import Types
    exposing
        ( Cursor(..)
        , Edge
        , EdgeId
        , Mode(Edit, Play)
        , Model
        , Msg(..)
        , Point
        , PointId
        )


init : ( Model, Cmd Msg )
init =
    ( { width = 0
      , height = 0
      , additiveSelection = False
      , cursor = Bored
      , edges = edgesForLevel 1
      , levelCodeModalActive = False
      , levelNumber = 1

      -- "Latch" whether this level has been solved, so the user can play around (and
      -- un-solve the puzzle) without having to re-solve to advance to the next level
      , levelSolved = False
      , mode = Edit --Play
      , points = pointsForLevel 1
      , selectedEdges = Dict.empty
      , selectedPoints = Dict.empty
      }
    , perform WindowSize Window.size
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateEdge from to ->
            let
                id =
                    toString from ++ "-" ++ toString to
            in
                { model | edges = Dict.insert id (Edge from to) model.edges } ! []

        Delete ->
            { model
                | cursor = Bored
                , edges =
                    -- first delete the edges that were selected
                    deleteEdges model.selectedEdges model.edges
                        -- then delete edges that reference deleted points
                        |> deleteEdgesWithPoints model.selectedPoints
                , points = deletePoints model.selectedPoints model.points
                , selectedEdges = Dict.empty
                , selectedPoints = Dict.empty
            }
                ! []

        KeyDown keyCode ->
            case Keyboard.Key.fromCode keyCode of
                Keyboard.Key.Shift _ ->
                    { model | additiveSelection = True } ! []

                _ ->
                    model ! []

        KeyUp keyCode ->
            case Keyboard.Key.fromCode keyCode of
                Keyboard.Key.Shift _ ->
                    { model | additiveSelection = False } ! []

                _ ->
                    model ! []

        MouseDown { x, y } ->
            let
                pointId =
                    findPointNear model.points (screenToPoint model x y)

                baseSelection =
                    if model.additiveSelection then
                        model.selectedPoints
                    else
                        Dict.empty
            in
                { model
                    | cursor =
                        pointId
                            |> Maybe.map Dragging
                            |> Maybe.withDefault Bored
                    , selectedPoints =
                        case pointId of
                            Just id ->
                                baseSelection
                                    |> Dict.update id (toggleMaybe ())

                            Nothing ->
                                baseSelection
                }
                    ! []

        MouseMove { x, y } ->
            let
                mousePoint =
                    screenToPoint model x y
            in
                case model.cursor of
                    Dragging id ->
                        (updateDragPoint model id mousePoint) ! []

                    _ ->
                        let
                            cursor =
                                findPointNear model.points mousePoint
                                    |> Maybe.map Hovering
                                    |> Maybe.withDefault Bored
                        in
                            { model | cursor = cursor } ! []

        MouseUp { x, y } ->
            { model
                | cursor =
                    case model.cursor of
                        Dragging id ->
                            Hovering id

                        value ->
                            value
                , levelSolved =
                    model.levelSolved
                        || List.length (allIntersections model.points model.edges)
                        == 0
            }
                ! []

        NextLevel ->
            { model
                | points = pointsForLevel (model.levelNumber + 1)
                , edges = edgesForLevel (model.levelNumber + 1)
                , levelNumber = model.levelNumber + 1
                , levelSolved = False
            }
                ! []

        NoOp ->
            model ! []

        EditMode ->
            { model | mode = Edit } ! []

        PlayMode ->
            { model | mode = Play } ! []

        ToggleLevelCodeModal ->
            { model
                | cursor = Bored
                , levelCodeModalActive = not model.levelCodeModalActive
            }
                ! []

        WindowSize { width, height } ->
            { model | width = width, height = height } ! []


deleteEdges : Dict EdgeId () -> Dict EdgeId Edge -> Dict EdgeId Edge
deleteEdges doomed edges =
    edges


deleteEdgesWithPoints : Dict PointId () -> Dict EdgeId Edge -> Dict EdgeId Edge
deleteEdgesWithPoints points edges =
    edges


deletePoints : Dict PointId () -> Dict PointId Point -> Dict PointId Point
deletePoints doomed points =
    points


toggleMaybe : a -> Maybe a -> Maybe a
toggleMaybe valueIfOn maybe =
    case maybe of
        Just _ ->
            Nothing

        Nothing ->
            Just valueIfOn


updateDragPoint : Model -> PointId -> Point -> Model
updateDragPoint model id newValue =
    let
        updater =
            Maybe.map (\_ -> newValue)
    in
        { model
            | points =
                Dict.update id updater model.points
        }
