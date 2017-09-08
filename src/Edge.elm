module Edge exposing (..)

import Array
import Tuple exposing (first, second)
import Math.Vector2 exposing (Vec2, add, dot, fromTuple, getX, getY, scale, sub, toTuple, vec2)
import Types
    exposing
        ( Edge
        , Point
        )


allIntersections : List Point -> List Edge -> List Point
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
        intersections allEdges


bisect : Point -> Point -> Point
bisect p q =
    let
        ( x1, y1 ) =
            toTuple p

        ( x2, y2 ) =
            toTuple q
    in
        fromTuple ( x1 + (x2 - x1) / 2, y1 + (y2 - y1) / 2 )



-- no cross product for Vec2 defined in Math.Vector2 :(


cross : Vec2 -> Vec2 -> Float
cross v w =
    (getX v) * (getY w) - (getY v) * (getX w)


fallbackPoint : Point
fallbackPoint =
    vec2 0 0


intersect : List Point -> Edge -> Edge -> Maybe Point
intersect points r s =
    let
        pointArray =
            Array.fromList points

        pointAt index =
            Array.get index pointArray
                |> Maybe.withDefault fallbackPoint
    in
        intersectSegments (pointAt r.from) (pointAt r.to) (pointAt s.from) (pointAt s.to)


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
                    else if 0 <= t_min && t_min <= 1 then
                        let
                            midpoint =
                                t_min + (1 - t_min) / 2
                        in
                            Just (add p (scale midpoint r))
                    else if 0 <= t_max && t_max <= 1 then
                        let
                            midpoint =
                                t_max / 2
                        in
                            Just (add p (scale midpoint r))
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


floatThreshold : Float
floatThreshold =
    1.0e-7


isZero : Float -> Bool
isZero x =
    abs x < floatThreshold



-- intersectVerticalSegments : Float -> Float -> Float -> Float -> Float -> Float -> Maybe Point
-- intersectVerticalSegments x01 y0 y1 x23 y2 y3 =
--     let
--         -- sort the y coordinates to make comparisons easier
--         ( a0, a1 ) =
--             if y0 <= y1 then
--                 ( y0, y1 )
--             else
--                 ( y1, y0 )
--         ( b0, b1 ) =
--             if y2 <= y3 then
--                 ( y2, y3 )
--             else
--                 ( y3, y2 )
--     in
--         -- If both segments are on the same column (~equal x coords), determine whether there is
--         -- over lap, and the "point" to label as the overlap (the center of the pieces that overlap)
--         if nearlyEqual x01 x23 then
--             -- 'a' contained within 'b'?
--             if b0 <= a0 && a1 <= b1 then
--                 Just ( x01, a0 + (a1 - a0) / 2 )
--                 -- 'b' contained within 'a'?
--             else if a0 <= b0 && b1 <= a1 then
--                 Just ( x01, b0 + (b1 - b0) / 2 )
--                 -- left side of 'a' within 'b'?
--             else if b0 <= a0 && a0 <= b1 then
--                 Just ( x01, a0 + (b1 - a0) / 2 )
--                 -- right side of 'a' within 'b'?
--             else if b0 <= a1 && a1 <= b1 then
--                 Just ( x01, b0 + (a1 - b0) / 2 )
--             else
--                 Nothing
--         else
--             Nothing


nearlyEqual : Float -> Float -> Bool
nearlyEqual x y =
    abs (x - y) < floatThreshold
