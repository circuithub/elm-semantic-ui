module SemanticUI.Elements.Divider exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


divider : Maybe String -> Html msg
divider text =
    div
        [ class "ui divider"
        , classList [ ( "horizontal", text /= Nothing ) ]
        ]
        (case text of
            Nothing ->
                []

            Just text ->
                [ Html.text text ]
        )
