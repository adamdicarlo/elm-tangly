module Subscriptions exposing (subscriptions)

import Keyboard
import Mouse
import Window
import Types exposing (Model, Mode(Edit), Msg(..))


{-| width and height of HUD must correspond to CSS
-}
hudWidth : Int
hudWidth =
    640


hudEditModeWidth : Int
hudEditModeWidth =
    900


hudHeight : Int
hudHeight =
    90


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        -- We only want to receive mouse events that happen within the playfield (not within
        -- the HUD)
        mouseEvent : (Mouse.Position -> Msg) -> Mouse.Position -> Msg
        mouseEvent message { x, y } =
            let
                width =
                    if model.mode == Edit then
                        hudEditModeWidth
                    else
                        hudWidth
            in
                if y >= (model.height - hudHeight) && x <= width then
                    NoOp
                else
                    message { x = x, y = y }
    in
        Sub.batch
            [ Keyboard.downs KeyDown
            , Keyboard.ups KeyUp
            , Mouse.downs <| mouseEvent MouseDown
            , Mouse.moves <| mouseEvent MouseMove
            , Mouse.ups <| mouseEvent MouseUp
            , Window.resizes WindowSize
            ]
