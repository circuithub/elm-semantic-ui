module SemanticUI.Elements.Container exposing (container)

{-|
A container is an element designed to contain page elements to a reasonable
maximum width based on the size of a user's screen. This is useful to couple
with other UI elements like a grid or menu to restrict their width to a
reasonable size for display.

# Viewing containers

@docs container
-}

import Html exposing (..)
import Html.Attributes exposing (..)


{-| View Html elements in a standard container.
-}
container : List (Html msg) -> Html msg
container =
    div [ class "ui container" ]
