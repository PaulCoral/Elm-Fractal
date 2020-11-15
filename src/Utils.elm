module Utils exposing (listToListOfPair)

{-| Takes a list a return a new list with pair of consecutive elements
-}
listToListOfPair : List a -> List (a,a)
listToListOfPair list =
    listToListOfPairRec list []

{-| Same as `listToListOfPair` with accumulator
-}
listToListOfPairRec : List a -> List (a,a) -> List (a,a)
listToListOfPairRec list acc =
    case list of
        [] -> acc
        [_] -> acc
        x :: xs ->
            case xs of
                y :: _ -> (x,y) :: acc