module SemanticUI exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


type Color
    = Blue
    | Teal
    | Yellow
    | Red


type Size
    = Mini
    | Tiny
    | Small
    | Medium
    | Large
    | Big
    | Massive
    | Huge


type Alignment
    = Centered


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
