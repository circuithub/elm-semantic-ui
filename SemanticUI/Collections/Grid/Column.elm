module SemanticUI.Collections.Grid.Column exposing (..)

{-| A column in a grid.

# Column properties

@docs Config, init

## Text alignment

A column can specify its text alignment.

@docs textAlignment

## Width

A column can vary in width taking up more than a single grid column.

@docs width
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (TextAlignment(..), ColumnCount(..), textAlignmentClass)


{-| The configuration of a column.
-}
type alias Config =
    { textAlignment : TextAlignment
    , width : ColumnCount
    }


{-| How text should be aligned in a column.
-}
textAlignment : TextAlignment -> Config -> Config
textAlignment textAlignment model =
    { model | textAlignment = textAlignment }


{-| Specify the width of a column.
-}
width : ColumnCount -> Config -> Config
width width model =
    { model | width = width }


{-| The simplest configuration of a column.
-}
init : Config
init =
    { textAlignment = LeftAligned
    , width = OneColumn
    }
