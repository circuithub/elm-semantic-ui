module SemanticUI.Modules.Common exposing (Render, WrappedContent, WrappedNode)

import Html exposing (..)


{-| Convenience alias for a HTML DOM Node render function
-}
type alias Render msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


{-| Convenience type alias for an `Html.Node` wrapper to augment node behaviour

It takes the following:

  - Node type as a render function e.g. `Html.div`
  - Extra attributes of node
  - The children of node

-}
type alias WrappedNode msg =
    Render msg -> List (Attribute msg) -> List (Html msg) -> Html msg


{-| Convenience type alias for an `Html.Node` wrapper to augment node behaviour
and only have specific content as children.

It takes the following:

  - Node type as a render function e.g. `Html.div`
  - Extra attributes of node
  - The children content

-}
type alias WrappedContent msg content =
    Render msg -> List (Attribute msg) -> content -> Html msg
