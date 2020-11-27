module PresetPattern exposing (presetList)

import FracPattern exposing (..)


type alias Preset =
    { name : String
    , pattern : String
    }


presetList : List Preset
presetList =
    [ Preset "Snow Flake" (anglesToString [ 0, 60, -120, 60 ])
    , Preset "Cool Cube" (anglesToString [ 0, 90, -90, -90, 90 ])
    , Preset "Many Cubes" (anglesToString [ 0, 90, 90, 90 ])
    , Preset "Fibonacci" (anglesToString (fibonacciList 3 9))
    , Preset "+Fibonacci" (anglesToString (fibonacciList 6 12))
    , Preset "++Fibonacci" (anglesToString (fibonacciList 9 15))
    ]


fibonacci : Int -> Float
fibonacci i =
    case i of
        0 ->
            0

        1 ->
            1

        _ ->
            fibonacci (i - 1) + fibonacci (i - 2)


fibonacciList : Int -> Int -> List Float
fibonacciList from to =
    let
        range =
            List.range from to
    in
    List.map fibonacci range
