module SemanticUI.Elements.List exposing (ul, item, ListItem)

{-|
A list is used to group related content.

# List items

Lists can only contain list items. To enforce this at the type level,
we use an opaque `ListItem` type, which can contain arbitray Html. Create
list items with `item`.

@docs ListItem, item

# Viewing lists

@docs ul
-}

import Html exposing (..)
import Html.Attributes exposing (..)


{-| An item in a list.
-}
type ListItem msg
    = ListItem (List (Html msg))


{-| Wrap Html as a list item.
-}
item : List (Html msg) -> ListItem msg
item =
    ListItem


{-| View a list of items as a `<ul>` element.
-}
ul : List (ListItem msg) -> Html msg
ul items =
    Html.ul [ class "ui list" ] (List.map (\(ListItem contents) -> li [] contents) items)
