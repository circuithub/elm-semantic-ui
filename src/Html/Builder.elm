module Html.Builder exposing
    ( Builder
    , prependAttribute, prependAttributes, appendAttribute, appendAttributes
    , prependChild, prependChildren, appendChild, appendChildren
    )

{-| Helper methods for building HTML.

@docs Builder


# Attributes

@docs prependAttribute, prependAttributes, appendAttribute, appendAttributes


# Child nodes

@docs prependChild, prependChildren, appendChild, appendChildren

-}

import Html exposing (..)


{-| Convenient alias for a HTML DOM element's render function, taking
a list of attributes and a list of children.
-}
type alias Builder msg =
    List (Attribute msg) -> List (Html msg) -> Html msg


{-| Transform a `Builder` to prepend multiple attributes.
-}
prependAttributes : List (Attribute msg) -> Builder msg -> Builder msg
prependAttributes extraAttrs element attrs =
    element (extraAttrs ++ attrs)


{-| Transform a `Builder` to prepend an attribute.
-}
prependAttribute : Attribute msg -> Builder msg -> Builder msg
prependAttribute extraAttr element attrs =
    element (extraAttr :: attrs)


{-| Transform a `Builder` to append multiple attributes.
-}
appendAttributes : List (Attribute msg) -> Builder msg -> Builder msg
appendAttributes extraAttrs element attrs =
    element (attrs ++ extraAttrs)


{-| Transform a `Builder` to append an attribute.
-}
appendAttribute : Attribute msg -> Builder msg -> Builder msg
appendAttribute extraAttr element attrs =
    element (attrs ++ [ extraAttr ])


{-| Transform a `Builder` to prepend multiple children.
-}
prependChildren : List (Html msg) -> Builder msg -> Builder msg
prependChildren extraChildren element attrs children =
    element attrs (extraChildren ++ children)


{-| Transform a `Builder` to prepend a child.
-}
prependChild : Html msg -> Builder msg -> Builder msg
prependChild extraChild element attrs children =
    element attrs (extraChild :: children)


{-| Transform a `Builder` to append multiple children.
-}
appendChildren : List (Html msg) -> Builder msg -> Builder msg
appendChildren extraChildren element attrs children =
    element attrs (children ++ extraChildren)


{-| Transform a `Builder` to append a child.
-}
appendChild : Html msg -> Builder msg -> Builder msg
appendChild extraChild element attrs children =
    element attrs (children ++ [ extraChild ])
