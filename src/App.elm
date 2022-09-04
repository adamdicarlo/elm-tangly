module App exposing (main)

import Browser
import Subscriptions exposing (subscriptions)
import Types exposing (Model, Msg)
import Update exposing (init, update)
import View exposing (view)


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
