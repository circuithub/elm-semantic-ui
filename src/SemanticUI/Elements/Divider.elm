module SemanticUI.Elements.Divider exposing (divider, hidden)

{-| A divider visually segments content into groups.

@docs divider, hidden

-}

import Html exposing (..)
import Html.Attributes exposing (..)


{-| View a horizontal divider, optionally containing some centered text.
-}
divider : Maybe String -> Html msg
divider text =
    div
        [ class "ui divider"
        , classList [ ( "horizontal", text /= Nothing ) ]
        ]
        (case text of
            Nothing ->
                []

            Just t ->
                [ Html.text t ]
        )


{-| A hidden divider divides content without creating a dividing line.
-}
hidden : Html msg
hidden =
    div [ class "ui hidden divider" ] []
