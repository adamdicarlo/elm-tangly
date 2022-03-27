module Update exposing (init, update)

import Browser.Dom exposing (Viewport, getViewport)
import Constants exposing (edgesForLevel, pointsForLevel)
import Dict exposing (Dict)
import Edge exposing (allIntersections)
import Keyboard
import Math.Vector2 exposing (Vec2, getX, getY, toRecord, vec2)
import Model exposing (findPointNear, isSelectionEmpty, screenToPoint)
import Task exposing (Task)
import Types
    exposing
        ( Cursor(..)
        , Edge
        , EdgeDict
        , EdgeId
        , Mode(..)
        , Model
        , Msg(..)
        , Point
        , PointDict
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
    , Task.perform WindowSize getWindowSize
    )


getWindowSize : Task x Vec2
getWindowSize =
    let
        viewportToSize : Viewport -> Vec2
        viewportToSize { viewport } =
            vec2 viewport.width viewport.height
    in
    Task.map viewportToSize getViewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateEdge from to ->
            let
                id =
                    from ++ "-" ++ to
            in
            ( { model | edges = Dict.insert id (Edge from to) model.edges }
            , Cmd.none
            )

        Delete ->
            ( { model
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
            , Cmd.none
            )

        KeyDown keyCode ->
            case keyCode of
                Keyboard.Shift ->
                    ( { model | additiveSelection = True }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        KeyUp keyCode ->
            case keyCode of
                Keyboard.Shift ->
                    ( { model | additiveSelection = False }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        MouseDown pos ->
            let
                location =
                    screenToPoint model (getX pos) (getY pos)

                maybePointId =
                    findPointNear model.points location

                baseSelection =
                    if model.additiveSelection then
                        model.selectedPoints

                    else
                        Dict.empty
            in
            case maybePointId of
                Just pointId ->
                    ( { model
                        | cursor = Dragging pointId
                        , selectedPoints =
                            Dict.update pointId (toggleMaybe ()) baseSelection
                      }
                    , Cmd.none
                    )

                Nothing ->
                    if model.mode == Edit && isSelectionEmpty model then
                        ( { model | cursor = Bored, points = insertPoint model.points location }
                        , Cmd.none
                        )

                    else
                        ( { model | cursor = Bored, selectedPoints = Dict.empty, selectedEdges = Dict.empty }
                        , Cmd.none
                        )

        MouseMove pos ->
            let
                { x, y } =
                    toRecord pos

                mousePoint =
                    screenToPoint model x y
            in
            case model.cursor of
                Dragging id ->
                    ( updateDragPoint model id mousePoint
                    , Cmd.none
                    )

                _ ->
                    let
                        cursor =
                            findPointNear model.points mousePoint
                                |> Maybe.map Hovering
                                |> Maybe.withDefault Bored
                    in
                    ( { model | cursor = cursor }
                    , Cmd.none
                    )

        MouseUp _ ->
            ( { model
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
            , Cmd.none
            )

        NextLevel ->
            ( { model
                | points = pointsForLevel (model.levelNumber + 1)
                , edges = edgesForLevel (model.levelNumber + 1)
                , levelNumber = model.levelNumber + 1
                , levelSolved = False
              }
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )

        EditMode ->
            ( { model | mode = Edit }
            , Cmd.none
            )

        PlayMode ->
            ( { model | mode = Play }
            , Cmd.none
            )

        ToggleLevelCodeModal ->
            ( { model
                | cursor = Bored
                , levelCodeModalActive = not model.levelCodeModalActive
              }
            , Cmd.none
            )

        WindowSize size ->
            ( { model | width = getX size, height = getY size }
            , Cmd.none
            )


insertPoint : PointDict -> Point -> PointDict
insertPoint points newPoint =
    let
        { x, y } =
            toRecord newPoint

        -- TODO: Use random string value here
        id =
            "p" ++ String.fromFloat x ++ "," ++ String.fromFloat y
    in
    Dict.insert id newPoint points


deleteEdges : Dict EdgeId () -> EdgeDict -> EdgeDict
deleteEdges doomed edges =
    edges


deleteEdgesWithPoints : Dict PointId () -> EdgeDict -> EdgeDict
deleteEdgesWithPoints points edges =
    edges


deletePoints : Dict PointId () -> PointDict -> PointDict
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
