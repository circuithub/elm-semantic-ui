module SemanticUI.Elements.List
    exposing
        ( Config
        , ListItem
        , attributes
        , div
        , divided
        , horizontal
        , init
        , item
        , relaxed
        , size
        , ul
        )

{-| A list is used to group related content.


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
import SemanticUI exposing (..)


{-| An item in a list.
-}
type ListItem msg
    = ListItem (List (Html msg))


type alias Config msg =
    { relaxed : Bool
    , divided : Bool
    , attributes : List (Attribute msg)
    , horizontal : Bool
    , size : Size
    }


init : Config msg
init =
    { relaxed = False
    , divided = False
    , attributes = []
    , horizontal = False
    , size = Medium
    }


horizontal : Bool -> Config msg -> Config msg
horizontal horizontal config =
    { config | horizontal = horizontal }


relaxed : Bool -> Config msg -> Config msg
relaxed relaxed config =
    { config | relaxed = relaxed }


divided : Bool -> Config msg -> Config msg
divided divided config =
    { config | divided = divided }


attributes : List (Attribute msg) -> Config msg -> Config msg
attributes attributes config =
    { config | attributes = attributes }


size : Size -> Config msg -> Config msg
size size config =
    { config | size = size }


{-| Wrap Html as a list item.
-}
item : List (Html msg) -> ListItem msg
item =
    ListItem


{-| View a list of items as a `<ul>` element.
-}
ul : Config msg -> List (ListItem msg) -> Html msg
ul =
    view Html.ul


{-| View a list of items as a `div` element.
-}
div : Config msg -> List (ListItem msg) -> Html msg
div =
    view Html.div


view element { attributes, relaxed, divided, horizontal, size } items =
    element
        (List.concat
            [ attributes
            , [ class "ui list"
              , classList
                    [ ( "relaxed", relaxed )
                    , ( "divided", divided )
                    , ( "horizontal", horizontal )
                    ]
              , sizeClass size
              ]
            ]
        )
        (List.map (\(ListItem i) -> Html.div [ class "item" ] i) items)
