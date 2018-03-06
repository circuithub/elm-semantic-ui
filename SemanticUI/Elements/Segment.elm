module SemanticUI.Elements.Segment exposing (..)

{-| A segment is used to create a grouping of related content.


# Viewing segments

@docs segment, raised


# Segment properties

@docs init, Config, attributes


## Raised

A segment may be formatted to raise above the page.

@docs raised


## Attached

A segment can be attached to other content on a page.

@docs attached


## Text alignment

A segment can have its text aligned to a side.

@docs textAlignment

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (Attached, TextAlignment(..), attachedClass)


{-| The configuration of a segment.
-}
type alias Config msg =
    { raised : Bool
    , attributes : List (Attribute msg)
    , attached : Maybe Attached
    , textAlignment : TextAlignment
    , clearing : Bool
    }


{-| The simplest configuration of a segment. Corresponds to `class="ui segment"`.
-}
init : Config msg
init =
    { raised = False
    , attributes = []
    , attached = Nothing
    , textAlignment = LeftAligned
    , clearing = False
    }


{-| Specify how text should be aligned in a segment.
-}
textAlignment : TextAlignment -> Config msg -> Config msg
textAlignment textAlignment model =
    { model | textAlignment = textAlignment }


{-| Specify whether or not a segment should appear raised.
-}
raised : Bool -> Config msg -> Config msg
raised raised model =
    { model | raised = raised }


{-| How to attach to surrounding content.
-}
attached : Maybe Attached -> Config msg -> Config msg
attached attached model =
    { model | attached = attached }


{-| Any other custom `Attribute`s to add to a segment. Custom attributes
will be added before `elm-semantic-ui` attributes.
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes attributes model =
    { model | attributes = attributes }


clearing : Bool -> Config msg -> Config msg
clearing c model =
    { model | clearing = c }


{-| View a segment with a particular configuration.
-}
segment : Config msg -> List (Html msg) -> Html msg
segment { raised, attributes, attached, textAlignment, clearing } =
    div
        (List.concat
            [ attributes
            , [ class "ui segment"
              , classList
                    [ ( "raised", raised )
                    , ( "clearing", clearing )
                    ]
              ]
            , case textAlignment of
                LeftAligned ->
                    []

                Centered ->
                    [ class "center aligned" ]
            , case attached of
                Just attached ->
                    [ attachedClass attached ]

                Nothing ->
                    []
            ]
        )
