module SemanticUI
    exposing
        ( Color(..)
        , Size(..)
        , Floated(..)
        , TextAlignment(..)
        , Attached(..)
        , attachedClass
        , sizeClass
        , floatedClass
        , textAlignmentClass
        )

{-|

# Common properties
@docs Attached, Color, Floated, Size, TextAlignment

# Forming Attributes
@docs attachedClass, floatedClass, sizeClass, textAlignmentClass

-}

import Html exposing (..)
import Html.Attributes exposing (..)


{-| Colours from the standard colour pallette in SemanticUI
-}
type Color
    = Blue
    | Teal
    | Yellow
    | Red


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


{-| Convert a `TextAlignment` into its corresponding `class` `Attribute`.
-}
textAlignmentClass : TextAlignment -> Attribute msg
textAlignmentClass textAlignment =
    case textAlignment of
        LeftAligned ->
            class ""

        Centered ->
            class "center aligned"


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
