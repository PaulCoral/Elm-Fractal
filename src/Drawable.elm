module Drawable exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
{-
    Represet a position in 2D drawing space
-}
type alias Point =
    { x : Int
    , y : Int
    }

lineStyle : List (Attribute msg)
lineStyle =
    [ stroke "blake"
    , strokeWidth "3"
    ]

{-
    Represent a sequence of Points
-}
type alias Points = List Point

pointPairToLine : Point -> Point -> Svg msg
pointPairToLine pt1 pt2 =
    let
        startX = String.fromInt pt1.x
        startY = String.fromInt pt1.y
        endX = String.fromInt pt2.x
        endY = String.fromInt pt2.y
    in
        line
            ([ x1 startX
            , y1 startY
            , x2 endX
            , y2 endY
            ] ++ lineStyle)
            []


pointsToLines : Points -> List (Svg msg)
pointsToLines points =
    pointsToLinesRec points []

pointsToLinesRec : Points -> List(Svg msg) -> List (Svg msg)
pointsToLinesRec points acc =
    case points of
        [] -> acc
        [_] -> acc
        p1 :: rest ->
            case rest of
                p2 :: _ ->
                    pointsToLinesRec rest ((pointPairToLine p1 p2) :: acc)