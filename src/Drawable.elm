module Drawable exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import FracPattern exposing (..)
import Utils exposing (..)



{-| Represent the state and the evolution of the drawing
-}
type alias DrawingState =
    { pattern : FracPattern
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


{-| Initialize the drawing state
-}
initDrawingState : DrawingState
initDrawingState =
    let
        nextPoint = pointAdd initialPoint (Point initLineLength 0)
        initialLine = (initialPoint, nextPoint)
    in
        DrawingState [] [initialLine]


{-| Add FracPattern to DrawingState
-}
addPatternToDrawingState : DrawingState -> FracPattern -> DrawingState
addPatternToDrawingState prev fracpat =
    {prev | pattern = fracpat}


{-| update drawing state to `iter` iterations
-}
updateDrawingState : DrawingState -> DrawingState
updateDrawingState ds =
    { ds | lines = (updateLinesTo ds) }
    {-if iter <= ds.currentIteration then
        ds
    else
        { ds
        | currentIteration = iter
        , lines = (updateLinesTo ds iter)
        }
    -}


{-| Rotate a vector from an angle
-}
vectorRotate : Vector -> Float -> Vector
vectorRotate v angle =
    let
        --newX = (cos angle) * v.x - (sin angle) * v.y
        --newY = (sin angle) * v.x - (cos angle) * v.y
        newX =  (cos (degrees angle)) * (vectorSize v)
        newY =  -(sin (degrees angle)) * (vectorSize v)
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


pointMultByScalar : Point -> Float -> Point
pointMultByScalar p s =
    Point (p.x * s) (p.y * s)


{-| Negate a point (e.g. (1,2) => (-1,-2))
-}
pointNeg : Point -> Point
pointNeg p =
    Point -p.x -p.y


{-| The turn angle used when drawing
-}
turnAngle : Float
turnAngle = 60


{-| The point where the drawing start
-}
initialPoint : Point
initialPoint = Point 0 (initLineLength / 2)


{-| Size of the line drawn at Iteration 0
-}
initLineLength : Float
initLineLength = 600


{-| The style that will be applied to every line
-}
lineStyle : List (Attribute msg)
lineStyle =
    [ fill "black"
    , stroke "black"
    , strokeWidth "3"
    ]


lineToVector : Line -> Vector
lineToVector l =
    let
        (p1,p2) = l
    in
        pointSub p2 p1


lineTranslate : Line -> Point -> Line
lineTranslate line p =
    let
        (p1, p2) = line
        newP1 = pointAdd p p1
        newP2 = pointAdd p p2
    in
        (newP1, newP2)


lineGetSize : Line -> Float
lineGetSize line =
    lineToVector line |> vectorSize


lineToSize : Line -> Float -> Line
lineToSize l newSize =
    let
        (p1, _) = l
        vector = lineToVector l
        scaled = vectorToSize vector newSize
        newP2 = pointAdd p1 scaled
    in
        (p1, newP2)


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
linesToSvgLines : Lines -> List (Svg msg)
linesToSvgLines lines =
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
getVectorFromPattern : Line -> PatternSymbol -> Vector
getVectorFromPattern line sym =
    updateVectorFromSymbol (lineToVector line) sym


{-| Get a new vector from PatternSymbol
-}
updateVectorFromSymbol : Vector -> PatternSymbol -> Vector
updateVectorFromSymbol v sym =
    case sym of
        Straight -> v
        Left -> vectorRotate v turnAngle
        Right -> vectorRotate v (-turnAngle)


vectorSize : Vector -> Float
vectorSize v =
    sqrt ((v.x * v.x) + (v.y * v.y))


vectorToSize : Vector -> Float -> Vector
vectorToSize v newSize =
    let
        originalSize = vectorSize v
        scale = newSize / originalSize
    in
        pointMultByScalar v scale


{-| Take DrawingState and a number of iterations. Return the lines after iterations
-}
updateLinesTo : DrawingState -> Lines
updateLinesTo ds =
    updateLinesToRec
        ds.pattern
        ds.lines

{-| Recursive implementation of `updateLinesTo`

    pat : The pattern to evolve with
    to : the number of iterations to reach
    current : the current iteration
    lines : An accumulator for the Lines to return
-}
updateLinesToRec : FracPattern -> Lines -> Lines
updateLinesToRec pat lines =
    let
        firstLine =
            case (List.head lines) of
                Just l -> l
                Nothing -> (initialPoint, initialPoint)
        newSize = Debug.log "Size : " ((lineGetSize firstLine) / 3)
        resizedLines = List.map (\n -> lineToSize n newSize) lines
    in
        (listFlatMap
            (\n -> updateLineWithPattern n pat)
            (resizedLines)
        )



updateLineWithPattern : Line -> FracPattern -> Lines
updateLineWithPattern line pat =
    let
        (p, _) = line
        vector = lineToVector line
    in
        updateLineWithPatternRec pat p vector []


updateLineWithPatternRec : FracPattern -> Point -> Vector -> Lines -> Lines
updateLineWithPatternRec pat prevPoint vector lines =
    case (Debug.log "before" pat) of
        [] -> (Debug.log "here" (List.reverse lines))
        x :: xs ->
            let
                vec = (updateVectorFromSymbol (Debug.log "sdf" vector) x)
                secPoint = Debug.log "secPoint" (pointAdd prevPoint vec)
                newLine = (prevPoint, secPoint)
            in
                updateLineWithPatternRec
                    (xs)
                    (secPoint)
                    (vec)
                    (newLine :: lines)
