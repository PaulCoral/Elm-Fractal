module PointSpace exposing (..)

import Canvas
import FracPattern exposing (..)



-- === POINT ===


{-| Represent a position in 2D drawing space
-}
type alias Point =
    { x : Float
    , y : Float
    }


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


pointToCanvasPoint : Point -> Canvas.Point
pointToCanvasPoint p =
    case p of
        { x, y } ->
            ( x, y )



-- === VECTOR ===


{-| Represent a vector in 2D drawing space
-}
type alias Vector =
    Point


{-| Rotate a vector from an angle
-}
vectorRotate : Vector -> DegreeAngle -> Vector
vectorRotate v angle =
    let
        deg =
            degrees angle

        newX =
            (cos deg * v.x) - (sin deg * v.y)

        newY =
            (sin deg * v.x) + (cos deg * v.y)

        --newX =  (cos (degrees angle)) * (vectorSize v)
        --newY =  -(sin (degrees angle)) * (vectorSize v)
    in
    { x = newX, y = newY }


{-| Get a new vector from PatternSymbol
-}
updateVectorFromSymbol : Vector -> DegreeAngle -> Vector
updateVectorFromSymbol v deg =
    vectorRotate v -deg


{-| Get the size of the given vector
-}
vectorSize : Vector -> Float
vectorSize v =
    sqrt ((v.x * v.x) + (v.y * v.y))


{-| Scale vector to the given size
-}
vectorToSize : Vector -> Float -> Vector
vectorToSize v newSize =
    let
        originalSize =
            vectorSize v

        scale =
            newSize / originalSize
    in
    pointMultByScalar v scale



-- === LINE ===


{-| A line, presented as two ORDERED Points
-}
type alias Line =
    ( Point, Point )


{-| Represent a sequence of Points
-}
type alias Lines =
    List Line


{-| Return a vector from line

    (point1,point2) = line
    Return vector of (point2 - point1)

-}
lineToVector : Line -> Vector
lineToVector l =
    let
        ( p1, p2 ) =
            l
    in
    pointSub p2 p1


{-| return length of a line
-}
lineGetSize : Line -> Float
lineGetSize line =
    lineToVector line |> vectorSize


{-| Scale a line to the given size
-}
lineToSize : Line -> Float -> Line
lineToSize l newSize =
    let
        ( p1, _ ) =
            l

        vector =
            lineToVector l

        scaled =
            vectorToSize vector newSize

        newP2 =
            pointAdd p1 scaled
    in
    ( p1, newP2 )
