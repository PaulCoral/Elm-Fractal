module Drawable exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import FracPattern exposing (..)




type alias DrawingState =
    { currentIteration : Int
    , pattern : FracPattern
    , lines : Lines
    }



{-| Represent a position in 2D drawing space
-}
type alias Point =
    { x : Float
    , y : Float
    }

{-| A line, presented as two ORDERED Points-}
type alias Line = (Point, Point)

{-| Represent a sequence of Points
-}
type alias Lines = List Line


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


{-| The turn angle used when drawing
-}
turnAngle : Float
turnAngle = 60


{-| Size of the line drawn at Iteration 0
-}
initLineLength : Float
initLineLength = 600


{-| The style that will be applied to every line
-}
lineStyle : List (Attribute msg)
lineStyle =
    [ stroke "blake"
    , strokeWidth "3"
    ]


{-| Create a Svg line from a line
-}
lineToSvgLine : Line -> Svg msg
lineToSvgLine l =
    let
        (pt1, pt2) = l
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
linestoSvgLines : Lines -> List (Svg msg)
linestoSvgLines lines =
    pointsToLinesRec lines []


{-| Create a sequence of Svg lines from a sequence of Points.
Recursive version. takes an accumulator.
-}
pointsToLinesRec : Lines -> List(Svg msg) -> List (Svg msg)
pointsToLinesRec lines acc =
    case lines of
        [] -> acc
        line :: rest ->
            pointsToLinesRec rest ((lineToSvgLine line) :: acc)


{-| Takes a line and a PatternSymbol, to return a new Point.
-}
getNewPointFromPattern : Line -> PatternSymbol -> Line
getNewPointFromPattern line sym =
    let
        (prev, pt) = line
        vector = pointSub pt prev
        updatedVec = updateVectorFromSymbol vector sym
    in
        (pt, (pointAdd pt updatedVec))


{-| Get a new vector from PatternSymbol
-}
updateVectorFromSymbol : Vector -> PatternSymbol -> Vector
updateVectorFromSymbol v sym =
    case sym of
        Straight -> v
        Left -> vectorRotate v turnAngle
        Right -> vectorRotate v (-turnAngle)
