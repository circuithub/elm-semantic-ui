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
        )

{-|

A header provides a short summary of content.

# Viewing headers

@docs h1, h2, h3, h4, h5

# Header properties

@docs Config, init, attributes

## Attachment

@docs attached

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (..)


{-| Configuration of a header.
-}
type alias Config msg =
    { attached : Maybe Attached
    , textAlignment : TextAlignment
    , attributes : List (Attribute msg)
    }


{-| The initial config of a header. Corresponds to just using `class="ui header"` in Semantic UI.
-}
init : Config msg
init =
    { attached = Nothing
    , textAlignment = LeftAligned
    , attributes = []
    }


{-| A header can be attached to other content, like a segment.
-}
attached : Maybe Attached -> Config msg -> Config msg
attached attached model =
    { model | attached = attached }


textAlignment : TextAlignment -> Config msg -> Config msg
textAlignment textAlignment model =
    { model | textAlignment = textAlignment }


{-| Any other custom `Attribute`s to add to this header. Custom attributes
 will be added before `elm-semantic-ui` attributes.
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes attrs model =
    { model | attributes = attrs }


viewAs :
    (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> Config msg
    -> List (Html msg)
    -> Html msg
viewAs element { attached, textAlignment, attributes } =
    element
        (List.concat
            [ attributes
            , [ class "ui header"
              , textAlignmentClass textAlignment
              ]
            , case attached of
                Just attached ->
                    [ attachedClass attached ]

                Nothing ->
                    []
            ]
        )


{-| View a header as a `<h1>` element.
-}
h1 : Config msg -> List (Html msg) -> Html msg
h1 =
    viewAs Html.h1


{-| View a header as a `<h2>` element.
-}
h2 : Config msg -> List (Html msg) -> Html msg
h2 =
    viewAs Html.h2


{-| View a header as a `<h3>` element.
-}
h3 : Config msg -> List (Html msg) -> Html msg
h3 =
    viewAs Html.h3


{-| View a header as a `<h4>` element.
-}
h4 : Config msg -> List (Html msg) -> Html msg
h4 =
    viewAs Html.h4


{-| View a header as a `<h5>` element.
-}
h5 : Config msg -> List (Html msg) -> Html msg
h5 =
    viewAs Html.h5
