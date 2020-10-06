module SemanticUI exposing
    ( Color(..), colorClass
    , Size(..), sizeClass
    , Floated(..), floatedClass
    , TextAlignment(..), textAlignmentClass
    , Attached(..), attachedClass
    , ColumnCount(..)
    )

{-| Common properties for other SemanticUI components.

@docs Color, colorClass
@docs Size, sizeClass
@docs Floated, floatedClass
@docs TextAlignment, textAlignmentClass
@docs Attached, attachedClass
@docs ColumnCount

-}

import Html exposing (..)
import Html.Attributes exposing (..)


{-| Colours from the standard colour palette in SemanticUI.
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


{-| Convert a `Color` into its corresponding `class` `Attribute`.
-}
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


{-| Convert `Attached` into a corresponding `class` `Attribute`.
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


{-| Column counts specify how wide a column is, or how many columns a grid
contains.
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
