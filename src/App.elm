module App exposing (..)

import Html exposing (Html)
import Edge exposing (allIntersections)
import Subscriptions exposing (subscriptions)
import Update exposing (init, update)
import Types exposing (Model, Msg)
import View exposing (view)


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
