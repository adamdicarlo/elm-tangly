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
        mouseEvent : (Mouse.Position -> Msg) -> Mouse.Position -> Msg
        mouseEvent message ({ x, y } as payload) =
            let
                width =
                    if model.mode == Edit then
                        hudEditModeWidth
                    else
                        hudWidth
            in
                -- We only want to receive mouse events:
                -- * that happen within the playfield (not within the HUD)
                -- * when the modal isn't active (check this here _just in case_ a delayed message
                --   comes through... shouldn't happen, but, you never know.
                if model.levelCodeModalActive || y >= (model.height - hudHeight) && x <= width then
                    NoOp
                else
                    message payload

        normalModeEvents =
            -- Prevent game input messages when modal is open
            if model.levelCodeModalActive then
                []
            else
                [ Keyboard.downs KeyDown
                , Keyboard.ups KeyUp
                , Mouse.downs <| mouseEvent MouseDown
                , Mouse.moves <| mouseEvent MouseMove
                , Mouse.ups <| mouseEvent MouseUp
                ]
    in
        Sub.batch <| Window.resizes WindowSize :: normalModeEvents
