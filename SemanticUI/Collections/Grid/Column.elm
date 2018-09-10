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
import SemanticUI exposing (ColumnCount(..), TextAlignment(..), textAlignmentClass)


{-| The configuration of a column.
-}
type alias Config msg =
    { textAlignment : TextAlignment
    , width : ColumnCount
    , attributes : List (Attribute msg)
    }


{-| How text should be aligned in a column.
-}
textAlignment : TextAlignment -> Config msg -> Config msg
textAlignment textAlignment model =
    { model | textAlignment = textAlignment }


{-| Specify the width of a column.
-}
width : ColumnCount -> Config msg -> Config msg
width width model =
    { model | width = width }


{-| Specify additional attributes for a column
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes attributes model =
    { model | attributes = attributes }


{-| The simplest configuration of a column.
-}
init : Config msg
init =
    { textAlignment = LeftAligned
    , width = OneColumn
    , attributes = []
    }
