module Html.Builder exposing (Builder, appendAttribute, appendAttributes, appendChild, appendChildren, prependAttribute, prependAttributes, prependChild, prependChildren)

import Html exposing (..)


{-| Convenient alias for a HTML DOM element's render function, taking
a list of attributes and a list of children.
-}
type alias Builder msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


prependAttributes : List (Attribute msg) -> Builder msg -> Builder msg
prependAttributes extraAttrs element attrs =
    element (extraAttrs ++ attrs)


prependAttribute : Attribute msg -> Builder msg -> Builder msg
prependAttribute extraAttr element attrs =
    element (extraAttr :: attrs)


appendAttributes : List (Attribute msg) -> Builder msg -> Builder msg
appendAttributes extraAttrs element attrs =
    element (attrs ++ extraAttrs)


appendAttribute : Attribute msg -> Builder msg -> Builder msg
appendAttribute extraAttr element attrs =
    element (attrs ++ [ extraAttr ])


prependChildren : List (Html msg) -> Builder msg -> Builder msg
prependChildren extraChildren element attrs children =
    element attrs (extraChildren ++ children)


prependChild : Html msg -> Builder msg -> Builder msg
prependChild extraChild element attrs children =
    element attrs (extraChild :: children)


appendChildren : List (Html msg) -> Builder msg -> Builder msg
appendChildren extraChildren element attrs children =
    element attrs (children ++ extraChildren)


appendChild : Html msg -> Builder msg -> Builder msg
appendChild extraChild element attrs children =
    element attrs (children ++ [ extraChild ])
