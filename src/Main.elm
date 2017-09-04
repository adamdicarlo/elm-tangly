module Main exposing (..)

import Array
import Color
import Html exposing (Html, text, div, img)
import Collage exposing (Form, LineStyle, collage, circle, defaultLine, move, outlined, rect, segment, traced)
import Element exposing (Element, toHtml)
import Task exposing (perform)
import Mouse
import Window


---- MODEL ----


type alias Point =
    ( Float, Float )


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


type alias Model =
    { width : Int
    , height : Int
    , points : List Point
    , edges : List Edge
    , cursor : Cursor
    }


fallbackPoint : Point
fallbackPoint =
    ( -9999, 9999 )


pointRadius : Float
pointRadius =
    10.0


level1 : { points : List Point, edges : List Edge }
level1 =
    { points =
        [ ( 0, 0 )
        , ( -400, -335 )
        , ( -350, 270 )
        , ( 200, -250 )
        , ( 30, 320 )
        ]
    , edges =
        [ Edge 0 1
        , Edge 0 2
        , Edge 1 2
        , Edge 2 3
        , Edge 3 4
        , Edge 0 4
        , Edge 1 4
        ]
    }


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



---- UPDATE ----


type Msg
    = MouseDown Mouse.Position
    | MouseMove Mouse.Position
    | MouseUp Mouse.Position
    | NoOp
    | WindowSize Window.Size


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



{- Zip up an index with each point: [(0, (x, y)), (1, (x, y)), ...] -}


indexPoints : List Point -> List ( Int, Point )
indexPoints points =
    List.map2 (,) (List.range 0 (List.length points - 1)) points


indexOfPointNear : List Point -> Point -> Maybe PointIndex
indexOfPointNear points test =
    let
        distance : Point -> Point -> Float
        distance ( x1, y1 ) ( x2, y2 ) =
            let
                square x =
                    x * x
            in
                sqrt (square (abs (x2 - x1)) + square (abs (y2 - y1)))

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
        ( (toFloat x) - xOrigin, yOrigin - (toFloat y) )



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        width =
            toFloat model.width

        height =
            toFloat model.height
    in
        List.concat
            [ viewPoints model
            , viewEdges model.edges model.points
            ]
            |> collage model.width model.height
            |> toHtml


highlighted : LineStyle
highlighted =
    { defaultLine | color = Color.red }


viewPoints : Model -> List Form
viewPoints model =
    let
        highlightIndex =
            case model.cursor of
                Bored ->
                    Nothing

                Hovering index ->
                    Just index

                Dragging index ->
                    Just index

        lineStyleForIndex index =
            case highlightIndex of
                Just highlightPointIndex ->
                    if highlightPointIndex == index then
                        highlighted
                    else
                        defaultLine

                _ ->
                    defaultLine

        viewPoint index ( x, y ) =
            circle pointRadius |> outlined (lineStyleForIndex index) |> move ( x, y )
    in
        List.indexedMap viewPoint model.points


viewEdges : List Edge -> List Point -> List Form
viewEdges edges points =
    let
        pointArray =
            Array.fromList points

        pointAt index =
            Array.get index pointArray
                |> Maybe.withDefault fallbackPoint

        viewEdge { from, to } =
            segment (pointAt from) (pointAt to) |> traced defaultLine
    in
        List.map viewEdge edges


pointByIndex : List Point -> Int -> Point
pointByIndex points index =
    case points of
        [] ->
            Debug.crash "Uh oh - bad point index"

        head :: tail ->
            case index of
                0 ->
                    head

                _ ->
                    pointByIndex tail (index - 1)



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.downs MouseDown
        , Mouse.moves MouseMove
        , Mouse.ups MouseUp
        , Window.resizes WindowSize
        ]
