module Subscriptions exposing (subscriptions)

import Mouse
import Window
import Types exposing (Model, Msg(..))


{-| width and height of HUD must correspond to CSS
-}
hudWidth =
    640


hudHeight =
    90


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        -- We only want to receive mouse events that happen within the playfield (not within
        -- the HUD)
        mouseEvent : (Mouse.Position -> Msg) -> Mouse.Position -> Msg
        mouseEvent message { x, y } =
            if y >= (model.height - hudHeight) && x <= hudWidth then
                NoOp
            else
                message { x = x, y = y }
    in
        Sub.batch
            [ Mouse.downs <| mouseEvent MouseDown
            , Mouse.moves <| mouseEvent MouseMove
            , Mouse.ups <| mouseEvent MouseUp
            , Window.resizes WindowSize
            ]
