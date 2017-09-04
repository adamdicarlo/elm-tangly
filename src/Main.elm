module Main exposing (..)

import Array
import Html exposing (Html, text, div, img)
import Collage exposing (Form, collage, circle, defaultLine, move, outlined, rect, segment, traced)
import Element exposing (Element, toHtml)
import Task exposing (perform)
import Window


---- MODEL ----


type alias Point =
    ( Float, Float )


type alias Edge =
    { from : Int
    , to : Int
    }


type alias Model =
    { width : Int
    , height : Int
    , points : List Point
    , edges : List Edge
    }


fallbackPoint =
    ( -9999, 9999 )


init : ( Model, Cmd Msg )
init =
    ( { width = 0
      , height = 0
      , points =
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
            ]
      }
    , perform WindowSize Window.size
    )



---- UPDATE ----


type Msg
    = WindowSize Window.Size
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        WindowSize { width, height } ->
            { model | width = width, height = height } ! []

        NoOp ->
            model ! []



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
            [ viewPoints model.points
            , viewEdges model.edges model.points
            ]
            |> collage model.width model.height
            |> toHtml


viewPoints : List Point -> List Form
viewPoints points =
    let
        viewPoint ( x, y ) =
            circle 8 |> outlined defaultLine |> move ( x, y )
    in
        List.map viewPoint points


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
    Sub.batch [ Window.resizes WindowSize ]
