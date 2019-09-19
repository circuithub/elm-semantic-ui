module SemanticUI.Modules.Common exposing (HtmlBuilder)

import Html exposing (..)


{-| Convenience alias for a HTML DOM Node render function
-}
type alias HtmlBuilder msg =
    List (Attribute msg) -> List (Html msg) -> Html msg
