module SemanticUI exposing
    ( Attached(..), Color(..), ColumnCount(..), Floated(..), Size(..), TextAlignment(..)
    , attachedClass, floatedClass, sizeClass, textAlignmentClass
    , colorClass
    )

{-|


# Common properties

@docs Attached, Color, ColumnCount, Floated, Size, TextAlignment


# Forming Attributes

@docs attachedClass, floatedClass, sizeClass, textAlignmentClass

-}

import Html exposing (..)
import Html.Attributes exposing (..)


{-| Colours from the standard colour pallette in SemanticUI
-}
type Color
    = Red
    | Orange
    | Yellow
    | Olive
    | Green
    | Teal
    | Blue
    | Violet
    | Purple
    | Pink
    | Brown
    | Grey
    | Black


colorClass : Color -> Attribute msg
colorClass color =
    class <|
        case color of
            Red ->
                "red"

            Orange ->
                "orange"

            Yellow ->
                "yellow"

            Olive ->
                "olive"

            Green ->
                "green"

            Teal ->
                "teal"

            Blue ->
                "blue"

            Purple ->
                "purple"

            Violet ->
                "violet"

            Pink ->
                "pink"

            Brown ->
                "brown"

            Black ->
                "black"

            Grey ->
                "grey"


{-| The standard sizes that most components use in SemanticUI.
-}
type Size
    = Mini
    | Tiny
    | Small
    | Medium
    | Large
    | Big
    | Massive
    | Huge


{-| Convert a `Size` into its corresponding `class` `Attribute`.
-}
sizeClass : Size -> Attribute msg
sizeClass size =
    classList
        [ ( "mini", size == Mini )
        , ( "tiny", size == Tiny )
        , ( "small", size == Small )
        , ( "medium", size == Medium )
        , ( "large", size == Large )
        , ( "big", size == Big )
        , ( "huge", size == Huge )
        , ( "massive", size == Massive )
        ]


{-| Some content can be floated.
-}
type Floated
    = FloatedRight


{-| Convert a `Floated` into its corresponding `class` `Attribute` .
-}
floatedClass : Floated -> Attribute msg
floatedClass floated =
    class <|
        case floated of
            FloatedRight ->
                "right floated"


{-| Specify how text is to be aligned.
-}
type TextAlignment
    = LeftAligned
    | Centered
    | RightAligned


{-| Convert a `TextAlignment` into its corresponding `class` `Attribute`.
-}
textAlignmentClass : TextAlignment -> Attribute msg
textAlignmentClass textAlignment =
    case textAlignment of
        LeftAligned ->
            class ""

        Centered ->
            class "center aligned"

        RightAligned ->
            class "right aligned"


{-| How content should attach to its adjacent content.
-}
type Attached
    = AttachTop
    | AttachMiddle
    | AttachBottom


{-| Convert `Attached` into a correspodning `class` `Attribute`.
-}
attachedClass : Attached -> Attribute msg
attachedClass side =
    class <|
        case side of
            AttachTop ->
                "top attached"

            AttachMiddle ->
                "middle attached"

            AttachBottom ->
                "bottom attached"


{-| Column counts, used to specify how wide a column is, or how many columns
a grid contains.
-}
type ColumnCount
    = OneColumn
    | TwoColumns
    | ThreeColumns
    | FourColumns
    | FiveColumns
    | SixColumns
    | SevenColumns
    | EightColumns
    | NineColumns
    | TenColumns
    | ElevenColumns
    | TwelveColumns
    | ThirteenColumns
    | FourteenColumns
    | FifteenColumns
    | SixteenColumns
