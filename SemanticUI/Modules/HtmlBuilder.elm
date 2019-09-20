module SemanticUI.Modules.HtmlBuilder exposing (HtmlBuilder, appendChild, appendChildren, prependAttribute, prependAttributes, prependChild, prependChildren)

import Html exposing (..)


{-| Convenience alias for a HTML DOM Node render function
-}
type alias HtmlBuilder msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


prependAttributes : List (Attribute msg) -> HtmlBuilder msg -> HtmlBuilder msg
prependAttributes extraAttrs element attrs =
    element (extraAttrs ++ attrs)


prependAttribute : Attribute msg -> HtmlBuilder msg -> HtmlBuilder msg
prependAttribute extraAttr element attrs =
    element (extraAttr :: attrs)


appendAttributes : List (Attribute msg) -> HtmlBuilder msg -> HtmlBuilder msg
appendAttributes extraAttrs element attrs =
    element (attrs ++ extraAttrs)


appendAttribute : Attribute msg -> HtmlBuilder msg -> HtmlBuilder msg
appendAttribute extraAttr element attrs =
    element (attrs ++ [ extraAttr ])


prependChildren : List (Html msg) -> HtmlBuilder msg -> HtmlBuilder msg
prependChildren extraChildren element attrs children =
    element attrs (extraChildren ++ children)


prependChild : Html msg -> HtmlBuilder msg -> HtmlBuilder msg
prependChild extraChild element attrs children =
    element attrs (extraChild :: children)


appendChildren : List (Html msg) -> HtmlBuilder msg -> HtmlBuilder msg
appendChildren extraChildren element attrs children =
    element attrs (children ++ extraChildren)


appendChild : Html msg -> HtmlBuilder msg -> HtmlBuilder msg
appendChild extraChild element attrs children =
    element attrs (children ++ [ extraChild ])
