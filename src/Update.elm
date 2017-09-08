module Update exposing (init, update)

import Task exposing (perform)
import Math.Vector2 exposing (Vec2, distance, fromTuple, toTuple, vec2)
import Window
import Constants exposing (level1, pointRadius)
import Types exposing (Cursor(..), Model, Msg(..), Point, PointIndex)


init : ( Model, Cmd Msg )
init =
    ( { width = 0
      , height = 0
      , points = level1.points
      , edges = level1.edges
      , cursor = Bored
      }
    , perform WindowSize Window.size
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDown { x, y } ->
            let
                nearPoint =
                    indexOfPointNear model.points (screenToPoint model x y)

                cursor =
                    nearPoint
                        |> Maybe.map Dragging
                        |> Maybe.withDefault Bored
            in
                { model | cursor = cursor } ! []

        MouseMove { x, y } ->
            let
                mousePoint =
                    screenToPoint model x y
            in
                case model.cursor of
                    Dragging index ->
                        (updateDragPoint model index mousePoint) ! []

                    _ ->
                        let
                            nearPoint =
                                indexOfPointNear model.points mousePoint

                            cursor =
                                nearPoint
                                    |> Maybe.map Hovering
                                    |> Maybe.withDefault Bored
                        in
                            { model | cursor = cursor } ! []

        MouseUp { x, y } ->
            { model
                | cursor =
                    case model.cursor of
                        Dragging index ->
                            Hovering index

                        value ->
                            value
            }
                ! []

        NoOp ->
            model ! []

        WindowSize { width, height } ->
            { model | width = width, height = height } ! []


indexPoints : List Point -> List ( Int, Point )
indexPoints points =
    -- Zip up an index with each point: [(0, (x, y)), (1, (x, y)), ...]
    List.map2 (,) (List.range 0 (List.length points - 1)) points


indexOfPointNear : List Point -> Point -> Maybe PointIndex
indexOfPointNear points test =
    let
        indexedDistance : ( Int, Point ) -> ( Int, Float )
        indexedDistance ( index, point ) =
            ( index, distance point test )

        closest =
            -- Determine distance between `test` and each point and sort distances
            List.map indexedDistance (indexPoints points)
                |> List.sortBy Tuple.second
                |> List.head
    in
        case closest of
            Nothing ->
                Nothing |> Debug.log "Bug! indexOfPointNear received empty list?"

            Just ( closestIndex, closestDistance ) ->
                if closestDistance < pointRadius then
                    Just closestIndex
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


updateDragPoint : Model -> PointIndex -> Point -> Model
updateDragPoint model index newValue =
    { model
        | points =
            List.concat
                [ List.take index model.points
                , [ newValue ]
                , List.drop (index + 1) model.points
                ]
    }
