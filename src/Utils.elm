module Utils exposing (listFlatMap)


{-| Perform a flatMap with the given function on the given list

This will perform a map on each element to a new list
and then flatten each new sublist

-}
listFlatMap : (a -> List b) -> List a -> List b
listFlatMap func list =
    List.foldl (++) ([]) (List.map func list)
