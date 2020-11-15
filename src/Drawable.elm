module Drawable exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import FracPattern exposing (..)

{-| Represet a position in 2D drawing space
-}
type alias Point =
    { x : Float
    , y : Float
    }

{-| Represent a vector in 2D drawing space
-}
type alias Vector = Point

{-| Rotate a vector from an angle
-}
vectorRotate : Vector -> Float -> Vector
vectorRotate v angle =
    let
        newX = (cos angle) * v.x - (sin angle) * v.y
        newY = (sin angle) * v.x - (cos angle) * v.y
    in
        { x = newX , y = newY }

{-| Add two points
-}
pointAdd : Point -> Point -> Point
pointAdd p1 p2 =
    Point (p1.x + p2.x) (p1.y + p2.y)

{-| Subtract two points
-}
pointSub : Point -> Point -> Point
pointSub p1 p2 =
    pointAdd p1 (pointNeg p2)

{-| Negate a point (e.g. (1,2) => (-1,-2))
-}
pointNeg : Point -> Point
pointNeg p =
    Point -p.x -p.y

{-| Represent a sequence of Points
-}
type alias Points = List Point

{-| The turn angle used when drawing
-}
turnAngle : Float
turnAngle = 60

{-| The style that will be applied to every line
-}
lineStyle : List (Attribute msg)
lineStyle =
    [ stroke "blake"
    , strokeWidth "3"
    ]

{-| Create a Svg line from two points
-}
pointPairToLine : Point -> Point -> Svg msg
pointPairToLine pt1 pt2 =
    let
        startX = String.fromFloat pt1.x
        startY = String.fromFloat pt1.y
        endX = String.fromFloat pt2.x
        endY = String.fromFloat pt2.y
    in
        line
            ([ x1 startX
            , y1 startY
            , x2 endX
            , y2 endY
            ] ++ lineStyle)
            []


{-| Create a sequence of Svg lines from a sequence of Points
-}
pointsToLines : Points -> List (Svg msg)
pointsToLines points =
    pointsToLinesRec points []

{-| Create a sequence of Svg lines from a sequence of Points.
Recursive version. takes an accumulator.
-}
pointsToLinesRec : Points -> List(Svg msg) -> List (Svg msg)
pointsToLinesRec points acc =
    case points of
        [] -> acc
        [_] -> acc
        p1 :: rest ->
            case rest of
                p2 :: _ ->
                    pointsToLinesRec rest ((pointPairToLine p1 p2) :: acc)

{-| Takes two points and a PatternSymbol, to return a new Point.
The first point is expected to follow the second
-}
getNewPointFromPattern : Point -> Point -> PatternSymbol -> Point
getNewPointFromPattern prev pt sym =
    let
        vector = pointSub pt prev
        updatedVec = updateVectorFromSymbol vector sym
    in
        pointAdd pt updatedVec

{-| Get a new vector from PatternSymbol
-}
updateVectorFromSymbol : Vector -> PatternSymbol -> Vector
updateVectorFromSymbol v sym =
    case sym of
        Straight -> v
        Left -> vectorRotate v turnAngle
        Right -> vectorRotate v (-turnAngle)
