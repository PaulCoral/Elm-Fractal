module Drawable exposing (..)

{-
    Represet a position in 2D drawing space
-}
type alias Point =
    { x : Int
    , y : Int
    }

{-
    Represent a sequence of Points
-}
type alias Points = List Point