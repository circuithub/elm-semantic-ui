module SemanticUI.Modules.Common exposing (HtmlRenderer)

import Html exposing (..)


{-| Convenience alias for a HTML DOM Node render function
-}
type alias HtmlRenderer msg =
    List (Attribute msg) -> List (Html msg) -> Html msg
