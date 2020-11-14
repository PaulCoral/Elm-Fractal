module FracPattern exposing (..)

{-
    Represent the possible "moves" in fractal drawing
-}
type PatternSymbol
    = Straight
    | Left
    | Right

{-
    Represent a sequence of patterns
-}
type alias FracPattern = List PatternSymbol

{-
    An empty FracPattern (i.e. with no PatternSymbol)
-}
emptyFracPattern : FracPattern
emptyFracPattern = []

{-
    Convert a Sequence of Pattern to a String
-}
fracPatternToString : FracPattern -> String
fracPatternToString fp =
    let
        mapString = List.map patternSymbolsToString fp
        sepComma = List.intersperse "," mapString
    in
        List.foldl (++) "" sepComma

{-
    Convert a pattern to a String
-}
patternSymbolsToString : PatternSymbol -> String
patternSymbolsToString sym =
    case sym of
        Straight -> "S"
        Left -> "L"
        Right -> "R"

{-
    Get a sequence of Pattern from a String
-}
fracPatternFromString : String -> FracPattern
fracPatternFromString str =
        List.map patternSymbolsFromChar
        <| String.toList str

{-
    Get a Pattern from a character. Ignore case
-}
patternSymbolsFromChar : Char -> PatternSymbol
patternSymbolsFromChar char =
    case (Char.toUpper char) of
        'L' -> Left
        'R' -> Right
        'S' -> Straight
        _   -> Straight