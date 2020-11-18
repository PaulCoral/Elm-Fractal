module Drawable exposing (..)

import Canvas

import FracPattern exposing (..)
import PointSpace exposing (..)
import Utils exposing (..)



{-| Represent the state and the evolution of the drawing
-}
type alias DrawingState =
    { pattern : Angles
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


{-| Add Angles to DrawingState
-}
addPatternToDrawingState : DrawingState -> Angles -> DrawingState
addPatternToDrawingState prev fracpat =
    {prev | pattern = fracpat}


{-| update drawing state lines
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
initLineLength = 800


{-| Create a Canvas pathSegment from Lines
-}
linesToShapeRec : Lines -> List Canvas.Shape -> List Canvas.Shape
linesToShapeRec lines acc =
    case lines of
        [] -> acc
        (p1, p2) :: xs ->
            let
                start = pointToCanvasPoint p1
                end = pointToCanvasPoint p2
                line =
                    (Canvas.path
                        start
                        [ Canvas.lineTo end ]
                    ) :: acc
            in
                linesToShapeRec xs line


{-| Create a sequence of Canvas lines from a sequence of Lines
-}
linesToShape : Lines -> List Canvas.Shape
linesToShape lines =
    linesToShapeRec lines []


{-| Take DrawingState Return the lines after an iteration
-}
updateLines : DrawingState -> Lines
updateLines ds =
    updateLinesRec
        ds.pattern
        ds.lines


{-| Recursive implementation
    pat : The pattern to evolve with
    lines : An accumulator for the Lines to return
-}
updateLinesRec : Angles -> Lines -> Lines
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


{-| Create a sequence of lines from previous line and a pattern of angles
-}
updateLineWithPattern : Line -> Angles -> Lines
updateLineWithPattern line pat =
    let
        (p, _) = line
        vector = lineToVector line
    in
        updateLineWithPatternRec pat p vector []


{-| The recursive implementation of `updateLineWithPattern`
-}
updateLineWithPatternRec : Angles -> Point -> Vector -> Lines -> Lines
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
