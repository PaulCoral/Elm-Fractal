module Utils exposing (..)

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
                [] -> acc
                y :: _ -> (x,y) :: acc


listFlatMap : (a -> List b) -> List a -> List b
listFlatMap func list =
    List.foldl (++) ([]) (List.map func list)


listLast : List a -> Maybe a
listLast list =
    List.head (List.reverse list)


listPutAtTheEnd : a -> List a -> List a
listPutAtTheEnd a list =
    List.reverse (a :: (List.reverse list))