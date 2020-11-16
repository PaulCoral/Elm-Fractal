module PresetPattern exposing (presetList)

import FracPattern exposing (..)

type alias Preset =
    { name : String
    , pattern : String
    }

presetList : List Preset
presetList =
    [ Preset "Snow Flake" (anglesToString [0,60,-120,60])
    , Preset "Cool Cube" (anglesToString [0,90,-90,-90,90])
    , Preset "Many Cubes"(anglesToString [0,90,90,90])
    ]
