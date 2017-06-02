module SemanticUI.Collections.Grid exposing (..)

{-| A grid is used to harmonize negative space in a layout.

# Grid API

The grid API adds a little more type safety by using the `Column` type.
A grid can only be viewed as a list of columns. To create a `Column`,
use the `column` smart constructor.

@docs Column, column

# Viewing grids

@docs grid

# Grid properties

@docs init, Config, attributes

## Padded grids

Since all grid columns include gutters, grids use negative margins to make sure
that the first and last columns sit flush with content outside the grid.

In some cases, like when a column or row is colored, you may want to avoid using
negative margins. You can do this by using a padded grid variation.

@docs padded

## Columns per row

By default, grids expect 16 columns per row, but this can be changed.

@docs columnsPerRow

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI.Collections.Grid.Column as Column
import SemanticUI exposing (ColumnCount(..), textAlignmentClass)


{-| A column in a grid.
-}
type Column msg
    = Column (Html msg)


{-| The configuration of a grid.
-}
type alias Config msg =
    { padded : Bool
    , columnsPerRow : ColumnCount
    , attributes : List (Attribute msg)
    }


{-| Any other custom `Attribute`s to add to this grid. Custom attributes
 will be added before `elm-semantic-ui` attributes.
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes attrs model =
    { model | attributes = attrs }


{-| The simplest configuration of a grid. Corresponds to `class="ui grid"`.
-}
init : Config msg
init =
    { padded = False
    , columnsPerRow = SixteenColumns
    , attributes = []
    }


{-| Whether or not this grid is padded.
-}
padded : Bool -> Config msg -> Config msg
padded padded model =
    { model | padded = padded }


{-| Indicate how many columns make up a row.
-}
columnsPerRow : ColumnCount -> Config msg -> Config msg
columnsPerRow columnsPerRow model =
    { model | columnsPerRow = columnsPerRow }


{-| View a list of columns as a grid with a particular configuration.
-}
grid : Config msg -> List (Column msg) -> Html msg
grid { padded, columnsPerRow, attributes } columns =
    div
        (List.concat
            [ attributes
            , [ class "ui grid"
              , class <|
                    case columnsPerRow of
                        OneColumn ->
                            "one column"

                        TwoColumns ->
                            "two column"

                        ThreeColumns ->
                            "three column"

                        FourColumns ->
                            "four column"

                        FiveColumns ->
                            "five column"

                        SixColumns ->
                            "six column"

                        SevenColumns ->
                            "seven column"

                        EightColumns ->
                            "eight column"

                        NineColumns ->
                            "nine column"

                        TenColumns ->
                            "ten column"

                        ElevenColumns ->
                            "eleven column"

                        TwelveColumns ->
                            "twelve column"

                        ThirteenColumns ->
                            "thirteen column"

                        FourteenColumns ->
                            "fourteen column"

                        FifteenColumns ->
                            "fifteen column"

                        SixteenColumns ->
                            "sixteen column"
              , classList
                    [ ( "padded", padded )
                    ]
              ]
            ]
        )
        (List.map (\(Column content) -> content)
            columns
        )


{-| Wrap Html as a grid column.
-}
column : Column.Config -> List (Html msg) -> Column msg
column { textAlignment, width } =
    Column
        << div
            (List.concat
                [ [ class "column"
                  , class <|
                        case width of
                            OneColumn ->
                                ""

                            TwoColumns ->
                                "two wide"

                            ThreeColumns ->
                                "three wide"

                            FourColumns ->
                                "four wide"

                            FiveColumns ->
                                "five wide"

                            SixColumns ->
                                "six wide"

                            SevenColumns ->
                                "seven wide"

                            EightColumns ->
                                "eight wide"

                            NineColumns ->
                                "nine wide"

                            TenColumns ->
                                "ten wide"

                            ElevenColumns ->
                                "eleven wide"

                            TwelveColumns ->
                                "twelve wide"

                            ThirteenColumns ->
                                "thirteen wide"

                            FourteenColumns ->
                                "fourteen wide"

                            FifteenColumns ->
                                "fifteen wide"

                            SixteenColumns ->
                                "sixteen wide"
                  , textAlignmentClass textAlignment
                  ]
                ]
            )
