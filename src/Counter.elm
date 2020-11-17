module Counter exposing (..)

import Time

{-| A counter to evolve the fractal

-}
type alias Counter =
    { upTo : Int
    , isEnabled : Bool
    }

initCounter : Counter
initCounter =
    Counter defaultCounterUpTo False


defaultCounterUpTo : Int
defaultCounterUpTo = 7


setCounter : Counter -> Bool -> Counter
setCounter counter enabled =
    { counter | isEnabled = enabled}


timeInterval : Float
timeInterval = 1500