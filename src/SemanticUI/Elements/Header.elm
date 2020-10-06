module SemanticUI.Elements.Header exposing
    ( h1, h2, h3, h4, h5, div
    , Config, init
    , attributes, icon, text, subheader, inverted, block, dividing
    , attached
    , textAlignment
    )

{-| A header provides a short summary of content.


# Viewing headers

@docs h1, h2, h3, h4, h5, div


# Header properties

@docs Config, init
@docs attributes, icon, text, subheader, inverted, block, dividing


## Attachment

A header can be attached to other content, like a segment.

@docs attached


## Text alignment

A header can have its text aligned to a side.

@docs textAlignment

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (..)
import SemanticUI.Elements.Icon as Icon


{-| Configuration of a header.
-}
type alias Config msg =
    { attached : Maybe Attached
    , textAlignment : TextAlignment
    , attributes : List (Attribute msg)
    , icon : Maybe Icon.Icon
    , text : Html msg
    , subheader : Maybe (Html msg)
    , inverted : Bool
    , block : Bool
    , dividing : Bool
    }


{-| The initial config of a header. Corresponds to just using `class="ui header"` in Semantic UI.
-}
init : Config msg
init =
    { attached = Nothing
    , textAlignment = LeftAligned
    , attributes = []
    , icon = Nothing
    , text = Html.text ""
    , subheader = Nothing
    , inverted = False
    , block = False
    , dividing = False
    }


{-| How a header should attach to surrounding content.
-}
attached : Maybe Attached -> Config msg -> Config msg
attached a model =
    { model | attached = a }


{-| Whether to invert the colours for extra contrast.
-}
inverted : Bool -> Config msg -> Config msg
inverted a model =
    { model | inverted = a }


{-| Whether to show the header inside a content block.
-}
block : Bool -> Config msg -> Config msg
block a model =
    { model | block = a }


{-| How to align the header text.
-}
textAlignment : TextAlignment -> Config msg -> Config msg
textAlignment a model =
    { model | textAlignment = a }


{-| Add an icon.
-}
icon : Maybe Icon.Icon -> Config msg -> Config msg
icon a model =
    { model | icon = a }


{-| Set the text contents of the header.
-}
text : Html msg -> Config msg -> Config msg
text a model =
    { model | text = a }


{-| Add a subheader, to be displayed underneath the main header text.
-}
subheader : Maybe (Html msg) -> Config msg -> Config msg
subheader a model =
    { model | subheader = a }


{-| Whether to show a divider between the header and the content below it.
-}
dividing : Bool -> Config msg -> Config msg
dividing a model =
    { model | dividing = a }


{-| Any other custom `Attribute`s to add to this header. Custom attributes
will be added before `elm-semantic-ui` attributes.
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes a model =
    { model | attributes = a }


viewAs :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> Config msg
    -> Html msg
viewAs element cfg =
    let
        content =
            List.concat
                [ [ cfg.text ]
                , case cfg.subheader of
                    Nothing ->
                        []

                    Just sub ->
                        [ Html.div [ class "sub header" ] [ sub ] ]
                ]
    in
    element
        (List.concat
            [ cfg.attributes
            , [ class "ui header"
              , textAlignmentClass cfg.textAlignment
              , classList
                    [ ( "inverted", cfg.inverted )
                    , ( "block", cfg.block )
                    , ( "dividing", cfg.dividing )
                    ]
              ]
            , case cfg.attached of
                Just a ->
                    [ attachedClass a ]

                Nothing ->
                    []
            ]
        )
        (case cfg.icon of
            Nothing ->
                content

            Just a ->
                [ Icon.icon Icon.init a
                , Html.div [ class "content" ] content
                ]
        )


{-| View a header as a `<div>` element.
-}
div : Config msg -> Html msg
div =
    viewAs Html.div


{-| View a header as a `<h1>` element.
-}
h1 : Config msg -> Html msg
h1 =
    viewAs Html.h1


{-| View a header as a `<h2>` element.
-}
h2 : Config msg -> Html msg
h2 =
    viewAs Html.h2


{-| View a header as a `<h3>` element.
-}
h3 : Config msg -> Html msg
h3 =
    viewAs Html.h3


{-| View a header as a `<h4>` element.
-}
h4 : Config msg -> Html msg
h4 =
    viewAs Html.h4


{-| View a header as a `<h5>` element.
-}
h5 : Config msg -> Html msg
h5 =
    viewAs Html.h5
