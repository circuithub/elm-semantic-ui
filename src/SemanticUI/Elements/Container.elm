module SemanticUI.Elements.Container exposing (container, textContainer, fluidContainer)

{-| A container is an element designed to contain page elements to a reasonable
maximum width based on the size of a user's screen. This is useful to couple
with other UI elements like a grid or menu to restrict their width to a
reasonable size for display.


# Viewing containers

@docs container, textContainer, fluidContainer

-}

import Html exposing (..)
import Html.Attributes exposing (..)


{-| View Html elements in a standard container.
-}
container : List (Html msg) -> Html msg
container =
    div [ class "ui container" ]


{-| View Html elements in a narrow container, suitable for text.
-}
textContainer : List (Html msg) -> Html msg
textContainer =
    div [ class "ui text container" ]


{-| View Html elements in a fluid container, taking up the width of the parent
element.
-}
fluidContainer : List (Html msg) -> Html msg
fluidContainer =
    div [ class "ui fluid container" ]
