module SemanticUI.Elements.List
    exposing
        ( Config
        , ListItem
        , attributes
        , bulleted
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
    , bulleted : Bool
    , size : Size
    }


init : Config msg
init =
    { relaxed = False
    , divided = False
    , attributes = []
    , horizontal = False
    , bulleted = False
    , size = Medium
    }


horizontal : Bool -> Config msg -> Config msg
horizontal a config =
    { config | horizontal = a }


bulleted : Bool -> Config msg -> Config msg
bulleted a config =
    { config | bulleted = a }


relaxed : Bool -> Config msg -> Config msg
relaxed a config =
    { config | relaxed = a }


divided : Bool -> Config msg -> Config msg
divided a config =
    { config | divided = a }


attributes : List (Attribute msg) -> Config msg -> Config msg
attributes a config =
    { config | attributes = a }


size : Size -> Config msg -> Config msg
size a config =
    { config | size = a }


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


view element cfg items =
    let
        listItemTag =
            if cfg.bulleted then
                Html.li
            else
                Html.div
    in
    element
        (List.concat
            [ cfg.attributes
            , [ class "ui list"
              , classList
                    [ ( "relaxed", cfg.relaxed )
                    , ( "divided", cfg.divided )
                    , ( "horizontal", cfg.horizontal )
                    ]
              , sizeClass cfg.size
              ]
            ]
        )
        (List.map (\(ListItem i) -> listItemTag [ class "item" ] i) items)
