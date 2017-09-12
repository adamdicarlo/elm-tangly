module Edge exposing (..)

import Dict exposing (Dict)
import Math.Vector2 exposing (Vec2, add, dot, fromTuple, getX, getY, scale, sub, toTuple, vec2)
import Types
    exposing
        ( Edge
        , EdgeDict
        , Point
        , PointDict
        )


allIntersections : PointDict -> EdgeDict -> List Point
allIntersections allPoints allEdges =
    let
        intersections : List Edge -> List Point
        intersections remainingEdges =
            case remainingEdges of
                [] ->
                    []

                edge :: [] ->
                    []

                edge :: rest ->
                    List.filterMap (intersect allPoints edge) rest
                        ++ intersections rest
    in
        intersections (Dict.values allEdges)


{-| No cross product for Vec2 defined in Math.Vector2 :(
-}
cross : Vec2 -> Vec2 -> Float
cross v w =
    (getX v) * (getY w) - (getY v) * (getX w)


fallbackPoint : Point
fallbackPoint =
    vec2 0 0


intersect : PointDict -> Edge -> Edge -> Maybe Point
intersect points r s =
    let
        pointAt id =
            Dict.get id points
                |> Maybe.withDefault fallbackPoint
    in
        if r.from == s.from || r.from == s.to || r.to == s.from || r.to == s.to then
            -- Ignore when both edges share a point; otherwise every single point is considered an
            -- intersection. But what if the edges overlap? Any two edges ("A" and "B") can only
            -- share at most a single point, so the non-shared point of one of them will be flagged
            -- as an intersection when the other edge _it_ is attached to is checked against A or B.
            Nothing
        else
            intersectSegments (pointAt r.from) (pointAt r.to) (pointAt s.from) (pointAt s.to)


{-| Find the intersection point of two line segments

    Uses the technique described at <https://stackoverflow.com/a/565282>.

-}
intersectSegments : Point -> Point -> Point -> Point -> Maybe Point
intersectSegments p pr q qs =
    let
        r =
            sub pr p

        s =
            sub qs q

        r_cross_s =
            cross r s

        q_minus_p =
            sub q p

        q_minus_p_cross_r =
            cross (q_minus_p) r

        q_minus_p_cross_s =
            cross (q_minus_p) s
    in
        case ( isZero r_cross_s, isZero q_minus_p_cross_r ) of
            ( True, True ) ->
                -- Lines are collinear. Figure out if they overlap.
                let
                    r_dot_r =
                        dot r r

                    t0 =
                        (dot q_minus_p r) / r_dot_r

                    t1 =
                        t0 + (dot s r) / r_dot_r

                    t_min =
                        min t0 t1

                    t_max =
                        max t0 t1
                in
                    if t_min < 0 && t_max > 1 then
                        -- One line is contained within the other; just use the midpoint of the
                        -- smaller line -- in this case, the first.
                        Just (add p (scale 0.5 r))
                    else if t_min >= 0 && t_max <= 1 then
                        -- One line is contained within the other; just use the midpoint of the
                        -- smaller line -- in this case, the second.
                        Just (add q (scale 0.5 s))
                    else if isBetween 0 1 t_min then
                        let
                            midpoint =
                                t_min + (1 - t_min) / 2
                        in
                            Just (add p (scale midpoint r))
                    else if isBetween 0 1 t_max then
                        Just (add p (scale (t_max / 2) r))
                    else
                        Nothing

            ( True, False ) ->
                -- Lines are parallel (and non-intersecting)
                Nothing

            ( False, _ ) ->
                -- Lines are not parallel
                let
                    t =
                        q_minus_p_cross_s / r_cross_s

                    u =
                        q_minus_p_cross_r / r_cross_s
                in
                    if 0 <= t && t <= 1 && 0 <= u && u <= 1 then
                        Just (add p (scale t r))
                    else
                        Nothing


isBetween : number -> number -> number -> Bool
isBetween min max value =
    clamp min max value == value


floatThreshold : Float
floatThreshold =
    1.0e-7


isZero : Float -> Bool
isZero x =
    abs x < floatThreshold


nearlyEqual : Float -> Float -> Bool
nearlyEqual x y =
    abs (x - y) < floatThreshold
