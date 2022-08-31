module Subscriptions exposing (subscriptions)

import Browser.Events exposing (onKeyDown, onKeyUp, onMouseDown, onMouseMove, onMouseUp, onResize)
import Json.Decode as D
import Keyboard exposing (Key(..))
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


keyEvent : Model -> (Key -> Msg) -> D.Decoder Msg
keyEvent model message =
    let
        pickMsg =
            \rawKey ->
                case Keyboard.modifierKey rawKey of
                    Just k ->
                        message k

                    Nothing ->
                        NoOp
    in
    if model.mode == Edit then
        D.map pickMsg Keyboard.eventKeyDecoder

    else
        D.fail "keys only used in edit mode"


mouseEvent : Model -> (Vec2 -> Msg) -> D.Decoder Msg
mouseEvent model message =
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


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        normalModeEvents =
            -- Prevent game input messages when modal is open
            if model.levelCodeModalActive then
                []

            else
                [ onKeyDown <| keyEvent model KeyDown
                , onKeyUp <| keyEvent model KeyUp
                , onMouseDown <| mouseEvent model MouseDown
                , onMouseMove <| mouseEvent model MouseMove
                , onMouseUp <| mouseEvent model MouseUp
                ]
    in
    Sub.batch <| onResize (\x y -> toSize x y |> WindowSize) :: normalModeEvents


toSize : Int -> Int -> Vec2
toSize x y =
    vec2 (toFloat x) (toFloat y)


decodePosition : D.Decoder Vec2
decodePosition =
    D.map2 vec2 (D.field "pageX" D.float) (D.field "pageY" D.float)
