module SemanticUI
    exposing
        ( Alignment(..)
        , Color(..)
        , Size(..)
        , Floated(..)
        , sizeClass
        , floatedClass
        )

{-|

# Common properties
@docs Alignment, Color, Floated, Size

# Forming Attributes
@docs floatedClass, sizeClass

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


{-| Text alignment
-}
type Alignment
    = Centered


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
