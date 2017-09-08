module Subscriptions exposing (subscriptions)

import Mouse
import Window
import Types exposing (Model, Msg(..))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.downs MouseDown
        , Mouse.moves MouseMove
        , Mouse.ups MouseUp
        , Window.resizes WindowSize
        ]
