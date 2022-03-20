module Subscriptions exposing (subscriptions)

import Browser.Events exposing (onKeyDown, onKeyUp, onMouseDown, onMouseMove, onMouseUp, onResize)
import Json.Decode as D
import Math.Vector2 exposing (Vec2, getX, getY, vec2)
import Types exposing (Mode(..), Model, Msg(..))


{-| width and height of HUD must correspond to CSS
-}
hudWidth : Float
hudWidth =
    640


hudEditModeWidth : Float
hudEditModeWidth =
    900


hudHeight : Float
hudHeight =
    90


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        mouseEvent : (Vec2 -> Msg) -> D.Decoder Msg
        mouseEvent message =
            let
                width =
                    if model.mode == Edit then
                        hudEditModeWidth

                    else
                        hudWidth
            in
            D.map
                (\pos ->
                    -- We only want to receive mouse events:
                    -- * that happen within the playfield (not within the HUD)
                    -- * when the modal isn't active (check this here _just in case_ a delayed message
                    --   comes through... shouldn't happen, but, you never know.
                    if
                        model.levelCodeModalActive
                            || getY pos
                            >= (model.height - hudHeight)
                            && getX pos
                            <= width
                    then
                        NoOp

                    else
                        message pos
                )
                decodePosition

        normalModeEvents =
            -- Prevent game input messages when modal is open
            if model.levelCodeModalActive then
                []

            else
                [ -- onKeyDown KeyDown
                  -- , onKeyUp KeyUp
                  onMouseDown <| mouseEvent MouseDown
                , onMouseMove <| mouseEvent MouseMove
                , onMouseUp <| mouseEvent MouseUp
                ]
    in
    Sub.batch <| onResize (\x y -> toSize x y |> WindowSize) :: normalModeEvents


toSize : Int -> Int -> Vec2
toSize x y =
    vec2 (toFloat x) (toFloat y)


decodePosition : D.Decoder Vec2
decodePosition =
    D.map2 vec2 (D.field "pageX" D.float) (D.field "pageY" D.float)
