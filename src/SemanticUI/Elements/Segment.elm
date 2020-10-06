module SemanticUI.Elements.Segment exposing
    ( segment
    , init, Config, attributes
    , raised
    , attached
    , textAlignment
    , clearing, loading
    )

{-| A segment is used to create a grouping of related content.


# Viewing segments

@docs segment


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

@docs clearing, loading

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (Attached, TextAlignment(..), attachedClass, textAlignmentClass)


{-| The configuration of a segment.
-}
type alias Config msg =
    { raised : Bool
    , attributes : List (Attribute msg)
    , attached : Maybe Attached
    , textAlignment : TextAlignment
    , clearing : Bool
    , loading : Bool
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
    , loading = False
    }


{-| Specify how text should be aligned in a segment.
-}
textAlignment : TextAlignment -> Config msg -> Config msg
textAlignment a model =
    { model | textAlignment = a }


{-| Specify whether or not a segment should appear raised.
-}
raised : Bool -> Config msg -> Config msg
raised a model =
    { model | raised = a }


{-| How to attach to surrounding content.
-}
attached : Maybe Attached -> Config msg -> Config msg
attached a model =
    { model | attached = a }


{-| Any other custom `Attribute`s to add to a segment. Custom attributes
will be added before `elm-semantic-ui` attributes.
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes a model =
    { model | attributes = a }


{-| TODO
-}
clearing : Bool -> Config msg -> Config msg
clearing c model =
    { model | clearing = c }


{-| TODO
-}
loading : Bool -> Config msg -> Config msg
loading c model =
    { model | loading = c }


{-| View a segment with a particular configuration.
-}
segment : Config msg -> List (Html msg) -> Html msg
segment cfg =
    div
        (List.concat
            [ cfg.attributes
            , [ class "ui segment"
              , classList
                    [ ( "raised", cfg.raised )
                    , ( "clearing", cfg.clearing )
                    , ( "loading", cfg.loading )
                    ]
              , textAlignmentClass cfg.textAlignment
              ]
            , case cfg.attached of
                Just a ->
                    [ attachedClass a ]

                Nothing ->
                    []
            ]
        )
