module SemanticUI.Collections.Grid exposing
    ( Column(..), mapColumn, column
    , grid
    , Config, init
    , attributes, equalWidth, divided, stackable, doubling
    , padded
    , columnsPerRow
    )

{-| A grid is used to harmonize negative space in a layout.


# Grid API

The grid API adds a little more type safety by using the `Column` type.
A grid can only be viewed as a list of columns. To create a `Column`,
use the `column` smart constructor.

@docs Column, mapColumn, column


# Viewing grids

@docs grid


# Grid properties

@docs Config, init
@docs attributes, equalWidth, divided, stackable, doubling


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
import SemanticUI exposing (ColumnCount(..), textAlignmentClass)
import SemanticUI.Collections.Grid.Column as Column


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
    , equalWidth : Bool
    , divided : Bool
    , stackable : Bool
    , doubling : Bool
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
    , equalWidth = False
    , divided = False
    , stackable = False
    , doubling = False
    }


{-| Whether or not this grid is padded.
-}
padded : Bool -> Config msg -> Config msg
padded a model =
    { model | padded = a }


{-| Whether or not this grid is divided.
-}
divided : Bool -> Config msg -> Config msg
divided a model =
    { model | divided = a }


{-| Whether or not this grid is doubles column widths for each device jump.
-}
doubling : Bool -> Config msg -> Config msg
doubling a model =
    { model | doubling = a }


{-| Whether or not this grid automatically stacks rows to a single column on mobile devices.
-}
stackable : Bool -> Config msg -> Config msg
stackable a model =
    { model | stackable = a }


{-| Whether or not to display all columns as the same width.
-}
equalWidth : Bool -> Config msg -> Config msg
equalWidth a model =
    { model | equalWidth = a }


{-| Indicate how many columns make up a row.
-}
columnsPerRow : ColumnCount -> Config msg -> Config msg
columnsPerRow a model =
    { model | columnsPerRow = a }


{-| View a list of columns as a grid with a particular configuration.
-}
grid : Config msg -> List (Column msg) -> Html msg
grid cfg columns =
    div
        (List.concat
            [ cfg.attributes
            , [ class "ui grid"
              , class <|
                    case cfg.columnsPerRow of
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
                    [ ( "padded", cfg.padded )
                    , ( "equal width", cfg.equalWidth )
                    , ( "divided", cfg.divided )
                    , ( "doubling", cfg.doubling )
                    , ( "stackable", cfg.stackable )
                    ]
              ]
            ]
        )
        (List.map (\(Column content) -> content)
            columns
        )


{-| Wrap Html as a grid column.
-}
column : Column.Config msg -> List (Html msg) -> Column msg
column cfg =
    Column
        << div
            (List.concat
                [ [ class "column"
                  , class <|
                        case cfg.width of
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
                  , textAlignmentClass cfg.textAlignment
                  ]
                , cfg.attributes
                ]
            )


{-| Map the message type of a column.
-}
mapColumn : (a -> b) -> Column a -> Column b
mapColumn f (Column html) =
    Column (Html.map f html)
