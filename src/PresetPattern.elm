module PresetPattern exposing (presetList)

import FracPattern exposing (..)

type alias Preset =
    { name : String
    , pattern : String
    }

presetList : List Preset
presetList =
    [ snowFlake
    , coolCube
    ]


snowFlake : Preset
snowFlake = Preset "Snow Flake" (anglesToString [0,60,-120,60])

coolCube : Preset
coolCube = Preset "Cool Cube" (anglesToString [0,90,-90,-90,90])