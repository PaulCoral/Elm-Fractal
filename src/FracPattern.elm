module FracPattern exposing (..)



{-| Represent the possible "moves" in fractal drawing
-}
type alias DegreeAngle = Float


{-| Represent a sequence of patterns
-}
type alias Angles = List DegreeAngle


{-| An empty angle list
-}
emptyAngles : Angles
emptyAngles = []


{-| Convert a Sequence of Pattern to a String
-}
anglesToString : Angles -> String
anglesToString fp =
    let
        mapString = List.map degreeAngleToString fp
        sepComma = List.intersperse "," mapString
    in
        List.foldr (++) "" sepComma


{-| Convert a pattern to a String
-}
degreeAngleToString : DegreeAngle -> String
degreeAngleToString deg = String.fromFloat deg


{-| Get a sequence of Pattern from a String
-}
anglesFromString : String -> Angles
anglesFromString str =
    let
        filtered = String.filter (\n -> (n /= ' ')) str
        splited = String.split "," filtered
    in
        List.map degreeAngleFromString splited


{-| Get an angle from a string. Ignore case
-}
degreeAngleFromString : String -> DegreeAngle
degreeAngleFromString str =
    case (String.toFloat str) of
        Just a ->
            a
        Nothing ->
            0

