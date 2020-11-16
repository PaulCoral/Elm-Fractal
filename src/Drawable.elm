module Drawable exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

import FracPattern exposing (..)
import PointSpace exposing (..)
import Utils exposing (..)



{-| Represent the state and the evolution of the drawing
-}
type alias DrawingState =
    { pattern : FracPattern
    , lines : Lines
    }


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
    { ds | lines = (updateLines ds) }


{-| The point where the drawing start
-}
initialPoint : Point
initialPoint = Point 0 (initLineLength / 2)


{-| Size of the line drawn at Iteration 0
-}
initLineLength : Float
initLineLength = 900


{-| The style that will be applied to every line
-}
lineStyle : List (Attribute msg)
lineStyle =
    [ fill "black"
    , stroke "black"
    , strokeWidth "1"
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
linesToSvgLines : Lines -> List (Svg msg)
linesToSvgLines lines =
    linesToSvgLinesRec lines []


{-| Create a sequence of Svg lines from a sequence of Points.
Recursive version. takes an accumulator.
-}
linesToSvgLinesRec : Lines -> List(Svg msg) -> List (Svg msg)
linesToSvgLinesRec lines acc =
    case lines of
        [] -> acc
        line :: rest ->
            linesToSvgLinesRec rest ((lineToSvgLine line) :: acc)



{-| Take DrawingState and a number of iterations. Return the lines after iterations
-}
updateLines : DrawingState -> Lines
updateLines ds =
    updateLinesRec
        ds.pattern
        ds.lines

{-| Recursive implementation of `updateLinesTo`

    pat : The pattern to evolve with
    to : the number of iterations to reach
    current : the current iteration
    lines : An accumulator for the Lines to return
-}
updateLinesRec : FracPattern -> Lines -> Lines
updateLinesRec pat lines =
    let
        firstLine =
            case (List.head lines) of
                Just l -> l
                Nothing -> (initialPoint, initialPoint)
        newSize = ((lineGetSize firstLine) / 3) -- TODO change 3
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
    case pat of
        [] -> List.reverse lines
        x :: xs ->
            let
                vec = updateVectorFromSymbol vector x
                secPoint = pointAdd prevPoint vec
                newLine = (prevPoint, secPoint)
            in
                updateLineWithPatternRec
                    (xs)
                    (secPoint)
                    (vec)
                    (newLine :: lines)
