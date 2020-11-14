module FracPattern exposing (..)

{-
    Represent the possible "moves" in fractal drawing
-}
type PatternSymbols
    = Straight
    | Left
    | Right

{-
    Represent a sequence of patterns
-}
type alias FracPattern = List PatternSymbols

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
patternSymbolsToString : PatternSymbols -> String
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
    Get a Pattern from a character
-}
patternSymbolsFromChar : Char -> PatternSymbols
patternSymbolsFromChar char =
    case char of
        'L' -> Left
        'R' -> Right
        'S' -> Straight
        _   -> Straight