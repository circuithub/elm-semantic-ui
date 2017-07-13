module SemanticUI.Elements.Header
    exposing
        ( h1
        , h2
        , h3
        , h4
        , h5
        , init
        , Config
        , attached
        , attributes
        , textAlignment
        , text
        , subheader
        , icon
        , inverted
        , block
        )

{-|

A header provides a short summary of content.

# Viewing headers

@docs h1, h2, h3, h4, h5

# Header properties

@docs Config, init, attributes

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
    }


{-| How a header should attach to surrounding content.
-}
attached : Maybe Attached -> Config msg -> Config msg
attached attached model =
    { model | attached = attached }


inverted : Bool -> Config msg -> Config msg
inverted inverted model =
    { model | inverted = inverted }


block : Bool -> Config msg -> Config msg
block block model =
    { model | block = block }


textAlignment : TextAlignment -> Config msg -> Config msg
textAlignment textAlignment model =
    { model | textAlignment = textAlignment }


icon : Maybe Icon.Icon -> Config msg -> Config msg
icon icon model =
    { model | icon = icon }


text : Html msg -> Config msg -> Config msg
text text model =
    { model | text = text }


subheader : Maybe (Html msg) -> Config msg -> Config msg
subheader subheader model =
    { model | subheader = subheader }


{-| Any other custom `Attribute`s to add to this header. Custom attributes
 will be added before `elm-semantic-ui` attributes.
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes attrs model =
    { model | attributes = attrs }


viewAs :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> Config msg
    -> Html msg
viewAs element { attached, textAlignment, attributes, icon, text, subheader, inverted, block } =
    let
        content =
            List.concat
                [ [ text ]
                , case subheader of
                    Nothing ->
                        []

                    Just text ->
                        [ div [ class "sub header" ] [ text ] ]
                ]
    in
        element
            (List.concat
                [ attributes
                , [ class "ui header"
                  , textAlignmentClass textAlignment
                  , classList
                        [ ( "inverted", inverted )
                        , ( "block", block )
                        ]
                  ]
                , case attached of
                    Just attached ->
                        [ attachedClass attached ]

                    Nothing ->
                        []
                ]
            )
            (case icon of
                Nothing ->
                    content

                Just icon ->
                    [ Icon.icon Icon.init icon
                    , div [ class "content" ] content
                    ]
            )


type HeaderContent
    = HeaderContent
        { headerText : String
        , subheading : Maybe String
        }


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
