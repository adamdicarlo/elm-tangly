module App exposing (main)

import Browser
import Browser.Dom exposing (Viewport, getViewport)
import Browser.Events
import Dict exposing (Dict)
import Edge exposing (allIntersections)
import GraphicSVG as Svg exposing (Color, LineType, Shape)
import GraphicSVG.Widget as Widget
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Json.Decode as Decode exposing (Decoder)
import Keyboard exposing (Key)
import Levels exposing (edgesForLevel, pointsForLevel)
import List.Extra as List
import Math.Vector2 as Vector2 exposing (Vec2, distanceSquared, getX, getY, toRecord, vec2)
import Set exposing (Set)
import Task exposing (Task)
import Types
    exposing
        ( Edge
        , EdgeDict
        , EdgeId
        , Point
        , PointDict
        , PointId
        )


type alias Model =
    { height : Float
    , width : Float
    , additiveSelection : Bool
    , cursor : Cursor
    , edges : EdgeDict
    , levelCodeModalActive : Bool
    , levelNumber : Int
    , levelSolved : Bool
    , mode : Mode
    , points : PointDict
    , selectedEdges : Set EdgeId
    , selectedPoints : Set PointId
    }


type Cursor
    = Bored
    | Hovering PointId
    | Dragging PointId


type Mode
    = Edit
    | Play


type Msg
    = CreateEdge PointId PointId
    | Delete
    | EditMode
    | KeyDown Key
    | KeyUp Key
    | MouseDown Vec2
    | MouseMove Vec2
    | MouseUp
    | NextLevel
    | NoOp
    | PlayMode
    | ToggleLevelCodeModal
    | WindowSize Vec2


pointRadius : Float
pointRadius =
    8


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( { width = 0
      , height = 0
      , additiveSelection = False
      , cursor = Bored
      , edges = edgesForLevel 1
      , levelCodeModalActive = False
      , levelNumber = 1

      -- "Latch" whether this level has been solved, so the user can play around (and
      -- un-solve the puzzle) without having to re-solve to advance to the next level
      , levelSolved = False
      , mode = Edit --Play
      , points = pointsForLevel 1
      , selectedEdges = Set.empty
      , selectedPoints = Set.empty
      }
    , Task.perform WindowSize getWindowSize
    )


getWindowSize : Task x Vec2
getWindowSize =
    let
        viewportToSize : Viewport -> Vec2
        viewportToSize { viewport } =
            vec2 viewport.width viewport.height
    in
    Task.map viewportToSize getViewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CreateEdge from to ->
            let
                id : PointId
                id =
                    from ++ "-" ++ to
            in
            ( { model | edges = Dict.insert id (Edge from to) model.edges }
            , Cmd.none
            )

        Delete ->
            ( { model
                | cursor = Bored
                , edges =
                    -- first delete the edges that were selected
                    deleteEdges model.selectedEdges model.edges
                        -- then delete edges that reference deleted points
                        |> deleteEdgesWithPoints model.selectedPoints
                , points = deletePoints model.selectedPoints model.points
                , selectedEdges = Set.empty
                , selectedPoints = Set.empty
              }
            , Cmd.none
            )

        KeyDown keyCode ->
            case keyCode of
                Keyboard.Shift ->
                    ( { model | additiveSelection = True }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        KeyUp keyCode ->
            case keyCode of
                Keyboard.Shift ->
                    ( { model | additiveSelection = False }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

        MouseDown pos ->
            let
                location : Point
                location =
                    screenToPoint model (getX pos) (getY pos)

                maybePointId : Maybe PointId
                maybePointId =
                    findPointNear model.points location
            in
            case maybePointId of
                Just pointId ->
                    let
                        baseSelection : Set PointId
                        baseSelection =
                            if model.additiveSelection then
                                model.selectedPoints

                            else
                                Set.empty
                    in
                    ( { model
                        | cursor = Dragging pointId
                        , selectedPoints =
                            if baseSelection |> Set.member pointId then
                                Set.remove pointId baseSelection

                            else
                                Set.insert pointId baseSelection
                      }
                    , Cmd.none
                    )

                Nothing ->
                    if model.mode == Edit && isSelectionEmpty model then
                        ( { model
                            | cursor = Bored
                            , points = insertPoint model.points location
                          }
                        , Cmd.none
                        )

                    else
                        ( { model
                            | cursor = Bored
                            , selectedPoints = Set.empty
                            , selectedEdges = Set.empty
                          }
                        , Cmd.none
                        )

        MouseMove pos ->
            let
                { x, y } =
                    toRecord pos

                mousePoint : Point
                mousePoint =
                    screenToPoint model x y
            in
            case model.cursor of
                Dragging id ->
                    ( updateDragPoint model id mousePoint
                    , Cmd.none
                    )

                _ ->
                    let
                        cursor : Cursor
                        cursor =
                            findPointNear model.points mousePoint
                                |> Maybe.map Hovering
                                |> Maybe.withDefault Bored
                    in
                    ( { model | cursor = cursor }
                    , Cmd.none
                    )

        MouseUp ->
            ( { model
                | cursor =
                    case model.cursor of
                        Dragging id ->
                            Hovering id

                        value ->
                            value
                , levelSolved =
                    model.levelSolved
                        || List.length (allIntersections model.points model.edges)
                        == 0
              }
            , Cmd.none
            )

        NextLevel ->
            ( { model
                | points = pointsForLevel (model.levelNumber + 1)
                , edges = edgesForLevel (model.levelNumber + 1)
                , levelNumber = model.levelNumber + 1
                , levelSolved = False
              }
            , Cmd.none
            )

        NoOp ->
            ( model
            , Cmd.none
            )

        EditMode ->
            ( { model | mode = Edit }
            , Cmd.none
            )

        PlayMode ->
            ( { model | mode = Play }
            , Cmd.none
            )

        ToggleLevelCodeModal ->
            ( { model
                | cursor = Bored
                , levelCodeModalActive = not model.levelCodeModalActive
              }
            , Cmd.none
            )

        WindowSize size ->
            ( { model | width = getX size, height = getY size }
            , Cmd.none
            )


insertPoint : PointDict -> Point -> PointDict
insertPoint points newPoint =
    let
        { x, y } =
            toRecord newPoint

        -- TODO: Use random string value here
        id : String
        id =
            "p" ++ String.fromFloat x ++ "," ++ String.fromFloat y
    in
    Dict.insert id newPoint points


deleteEdges : Set EdgeId -> EdgeDict -> EdgeDict
deleteEdges _ edges =
    -- TODO
    edges


deleteEdgesWithPoints : Set PointId -> EdgeDict -> EdgeDict
deleteEdgesWithPoints _ edges =
    -- TODO
    edges


deletePoints : Set PointId -> PointDict -> PointDict
deletePoints _ points =
    -- TODO
    points


updateDragPoint : Model -> PointId -> Point -> Model
updateDragPoint model id newValue =
    let
        updater : Maybe a -> Maybe Point
        updater =
            Maybe.map (\_ -> newValue)
    in
    { model
        | points =
            Dict.update id updater model.points
    }


findPointNear : Dict PointId Point -> Point -> Maybe PointId
findPointNear points test =
    let
        distanceWithId : ( PointId, Point ) -> ( PointId, Float )
        distanceWithId ( id, point ) =
            ( id, distanceSquared point test )

        ifCloseEnough : ( a, Float ) -> Maybe a
        ifCloseEnough ( id, dist2 ) =
            if sqrt dist2 < pointRadius then
                Just id

            else
                Nothing
    in
    -- Determine distance between `test` and each point and keep smallest
    points
        |> Dict.toList
        |> List.map distanceWithId
        |> List.minimumBy Tuple.second
        |> Maybe.andThen ifCloseEnough


isSelectionEmpty : Model -> Bool
isSelectionEmpty model =
    Set.isEmpty model.selectedEdges && Set.isEmpty model.selectedPoints


edgeExists : Model -> PointId -> PointId -> Bool
edgeExists model p1 p2 =
    Dict.values model.edges
        |> List.any
            (\{ from, to } ->
                (from == p1 && to == p2)
                    || (from == p2 && to == p1)
            )


screenToPoint : Model -> Float -> Float -> Point
screenToPoint model x y =
    let
        xOrigin : Float
        xOrigin =
            model.width / 2.0

        yOrigin : Float
        yOrigin =
            model.height / 2.0
    in
    vec2 (x - xOrigin) (yOrigin - y)


toTuple : Point -> ( Float, Float )
toTuple p =
    ( Vector2.getX p, Vector2.getY p )


fallbackPoint : Point
fallbackPoint =
    vec2 -9999 9999



-- GAME BOARD VIEW


view : Model -> Html Msg
view ({ edges, points, width, height } as model) =
    let
        intersections : List Point
        intersections =
            allIntersections points edges

        canvas : Html a
        canvas =
            List.concat
                [ viewEdges model
                , viewPoints model
                , viewIntersections model intersections
                ]
                |> Widget.icon "gfx" width height

        canvasStyleAttrs : List (Html.Attribute msg)
        canvasStyleAttrs =
            case model.cursor of
                Bored ->
                    if model.mode == Edit && isSelectionEmpty model then
                        -- indicate that clicking will create a vertex
                        [ Attributes.style "cursor" "cell" ]

                    else
                        []

                Hovering _ ->
                    [ Attributes.style "cursor" "pointer" ]

                Dragging _ ->
                    [ Attributes.style "cursor" "move" ]
    in
    Html.div (Attributes.class "tangly" :: canvasStyleAttrs)
        [ canvas
        , viewHUD model intersections
        , viewLevelCodeModal model
        ]


hoverDragColor : Color
hoverDragColor =
    Svg.green


normalColor : Color
normalColor =
    Svg.black


selectedColor : Color
selectedColor =
    Svg.red


selectionLineType : LineType
selectionLineType =
    Svg.solid 2


styleForPointId : Model -> PointId -> Color
styleForPointId model id =
    let
        chooseColor : PointId -> Color
        chooseColor hoverDragPointId =
            -- Hovering or dragging takes highlight precedence
            if hoverDragPointId == id then
                hoverDragColor

            else
                normalColor
    in
    case model.cursor of
        Bored ->
            normalColor

        Hovering id_ ->
            chooseColor id_

        Dragging id_ ->
            chooseColor id_


viewPoints : Model -> List (Shape Never)
viewPoints model =
    let
        points : List ( PointId, Vector2.Vec2 )
        points =
            Dict.toList model.points

        viewPoint : ( PointId, Point ) -> Shape Never
        viewPoint ( id, point ) =
            Svg.circle pointRadius
                |> Svg.filled (styleForPointId model id)
                |> Svg.move (toTuple point)

        viewSelection : ( PointId, Point ) -> Maybe (Shape Never)
        viewSelection ( id, point ) =
            if Set.member id model.selectedPoints then
                Svg.rect (2 * pointRadius + 4) (2 * pointRadius + 4)
                    |> Svg.outlined selectionLineType selectedColor
                    |> Svg.move (toTuple point)
                    |> Just

            else
                Nothing
    in
    List.concat
        [ List.map viewPoint points
        , List.filterMap viewSelection points
        ]


viewEdges : Model -> List (Shape Never)
viewEdges model =
    let
        pointById : PointId -> ( Float, Float )
        pointById id =
            Dict.get id model.points
                |> Maybe.withDefault fallbackPoint
                |> toTuple

        viewEdge : { from : PointId, to : PointId } -> Shape Never
        viewEdge { from, to } =
            Svg.line (pointById from) (pointById to)
                |> Svg.outlined (Svg.solid 2) Svg.purple
    in
    Dict.values model.edges |> List.map viewEdge


viewIntersections : Model -> List Point -> List (Shape Never)
viewIntersections _ intersections =
    let
        viewIntersection : Point -> Shape Never
        viewIntersection point =
            Svg.circle 4 |> Svg.outlined (Svg.solid 1) Svg.red |> Svg.move (toTuple point)
    in
    List.map viewIntersection intersections



-- HUD


pluralize : String -> String -> Int -> String
pluralize singular plural count =
    String.join " "
        [ String.fromInt count
        , if count == 1 then
            singular

          else
            plural
        ]


viewHUD : Model -> List Point -> Html Msg
viewHUD model intersections =
    let
        count : Int
        count =
            List.length intersections

        level : Html Msg
        level =
            Html.div [ Attributes.class "level" ]
                [ String.concat
                    [ "Level "
                    , String.fromInt model.levelNumber
                    ]
                    |> Html.text
                ]

        progress : Html Msg
        progress =
            Html.div [ Attributes.class "progress" ]
                [ pluralize "intersection" "intersections" count
                    |> Html.text
                ]

        status : Html Msg
        status =
            Html.div [ Attributes.class "statusContainer" ]
                [ level
                , progress
                ]
    in
    Html.div
        [ Attributes.classList
            [ ( "hud", True )
            , ( "editMode", model.mode == Edit )
            ]
        ]
        [ status
        , viewHUDActions model
        ]


viewHUDActions : Model -> Html Msg
viewHUDActions model =
    Html.div [ Attributes.class "action" ]
        (case model.mode of
            Play ->
                if model.levelSolved then
                    [ iconLabel "🎊" "Solved!"
                    , nextLevelButton
                    ]

                else
                    [ editButton ]

            Edit ->
                List.append [ levelCodeButton, playButton ] <|
                    if isSelectionEmpty model then
                        []

                    else
                        [ deleteButton, createEdgeButton model ]
        )


iconLabel : String -> String -> Html Msg
iconLabel icon label =
    Html.span []
        [ Html.span [ Attributes.class "icon" ] [ Html.text icon ]
        , Html.span [] [ Html.text label ]
        ]


createEdgeButton : Model -> Html Msg
createEdgeButton model =
    case Set.toList model.selectedPoints of
        -- Exactly two vertices should be selected
        from :: to :: [] ->
            if edgeExists model from to then
                Html.button
                    [ Attributes.class "small gray disabled btn-3d"
                    , Attributes.disabled True
                    ]
                    [ iconLabel "🌟" "Create Edge" ]

            else
                Html.button
                    [ Attributes.class "small green btn-3d"
                    , Events.onClick <| CreateEdge from to
                    ]
                    [ iconLabel "🌟" "Create Edge" ]

        _ ->
            Html.span [] []


editButton : Html Msg
editButton =
    Html.button
        [ Attributes.class "small blue btn-3d"
        , Events.onClick EditMode
        ]
        [ iconLabel "🔧" "Edit" ]


levelCodeButton : Html Msg
levelCodeButton =
    Html.button
        [ Attributes.class "small green btn-3d"
        , Events.onClick ToggleLevelCodeModal
        ]
        [ iconLabel "👾" "Code" ]


nextLevelButton : Html Msg
nextLevelButton =
    Html.button
        [ Attributes.class "btn-3d red"
        , Events.onClick NextLevel
        ]
        [ Html.text "Next level →" ]


deleteButton : Html Msg
deleteButton =
    Html.button
        [ Attributes.class "small red btn-3d"
        , Events.onClick Delete
        ]
        [ iconLabel "💀" "Delete" ]


playButton : Html Msg
playButton =
    Html.button
        [ Attributes.class "small blue btn-3d"
        , Events.onClick PlayMode
        ]
        [ iconLabel "🎮" "Play" ]



-- LEVEL CODE MODAL


viewLevelCodeModal : Model -> Html Msg
viewLevelCodeModal model =
    if model.levelCodeModalActive then
        Html.div [ Attributes.class "levelCodeModalBackdrop open" ]
            [ Html.div [ Attributes.class "levelCodeModal" ]
                [ Html.pre []
                    [ Html.text <| levelToCode model
                    ]
                , Html.button
                    [ Attributes.class "btn-3d green"
                    , Events.onClick ToggleLevelCodeModal
                    ]
                    [ Html.text "OK" ]
                ]
            ]

    else
        Html.div [ Attributes.class "levelCodeModalBackdrop" ] []


format3 : String -> String -> String -> String -> String
format3 format a b c =
    case format |> String.split "%" of
        w :: x :: y :: [ z ] ->
            [ w, a, x, b, y, c, z ] |> String.concat

        _ ->
            [ "Bug in format passed to format3:"
            , format
            ]
                |> String.concat


showEdge : ( EdgeId, Edge ) -> String
showEdge ( id, { from, to } ) =
    format3
        "( \"%\", Edge % % )"
        id
        from
        to


showPoint : ( PointId, Point ) -> String
showPoint ( id, p ) =
    let
        ( x, y ) =
            toTuple p
    in
    format3
        "( \"%\", vec2 % % )"
        id
        (String.fromFloat x)
        (String.fromFloat y)


joinCodeLines : String -> String -> String
joinCodeLines a b =
    [ a
    , if String.isEmpty b then
        ""

      else
        "\n            , "
    , b
    ]
        |> String.concat


levelToCode : Model -> String
levelToCode model =
    let
        varName : String
        varName =
            "level" ++ String.fromInt model.levelNumber

        points : String
        points =
            Dict.toList model.points
                |> List.map showPoint
                |> List.foldl joinCodeLines ""

        edges : String
        edges =
            Dict.toList model.edges
                |> List.map showEdge
                |> List.foldl joinCodeLines ""
    in
    [ varName ++ " : Level"
    , varName ++ " ="
    , "    { edges ="
    , "        Dict.fromList"
    , "            [ " ++ edges
    , "            ]"
    , "    , points ="
    , "        Dict.fromList"
    , "            [ " ++ points
    , "            ]"
    , "    }"
    ]
        |> String.join "\n"



-- SUBSCRIPTIONS


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


keyEvent : Model -> (Key -> Msg) -> Decoder Msg
keyEvent model message =
    if model.mode == Edit then
        Keyboard.eventKeyDecoder
            |> Decode.andThen
                (\rawKey ->
                    case Keyboard.modifierKey rawKey of
                        Just k ->
                            Decode.succeed (message k)

                        Nothing ->
                            Decode.fail "ignored"
                )

    else
        Decode.fail "keys only used in edit mode"


mouseEvent : Model -> (Vec2 -> Msg) -> Decoder Msg
mouseEvent model message =
    let
        width : Float
        width =
            if model.mode == Edit then
                hudEditModeWidth

            else
                hudWidth
    in
    decodePosition
        |> Decode.andThen
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
                    Decode.fail "ignored"

                else
                    Decode.succeed (message pos)
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        normalModeEvents : List (Sub Msg)
        normalModeEvents =
            -- Prevent game input messages when modal is open
            if model.levelCodeModalActive then
                []

            else
                [ Browser.Events.onKeyDown <| keyEvent model KeyDown
                , Browser.Events.onKeyUp <| keyEvent model KeyUp
                , Browser.Events.onMouseDown <| mouseEvent model MouseDown
                , Browser.Events.onMouseMove <| mouseEvent model MouseMove
                , Browser.Events.onMouseUp <| mouseEvent model (always MouseUp)
                ]
    in
    Sub.batch <| Browser.Events.onResize (\x y -> toSize x y |> WindowSize) :: normalModeEvents


toSize : Int -> Int -> Vec2
toSize x y =
    vec2 (toFloat x) (toFloat y)


decodePosition : Decoder Vec2
decodePosition =
    Decode.map2 vec2
        (Decode.field "pageX" Decode.float)
        (Decode.field "pageY" Decode.float)
