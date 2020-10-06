module SemanticUI.Elements.List.Item exposing (ListItem, item, toHtml, Config, icon, iconConfig, init)

{-|


# List items

Lists can only contain list items. To enforce this at the type level,
we use an opaque `ListItem` type, which can contain arbitray Html. Create
list items with `item`.

@docs ListItem, item, toHtml, Config, icon, iconConfig, init

-}

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (..)
import SemanticUI.Elements.Icon as Icon


{-| An item in a list.
-}
type ListItem msg
    = ListItem (Config msg) (List (Html msg))


{-| Properties of a list item.
-}
type alias Config msg =
    { icon : Maybe Icon.Icon
    , iconCfg : Maybe (Icon.Config msg)
    }


{-| A list item with the simplest configuration.
-}
init : Config msg
init =
    { icon = Nothing
    , iconCfg = Nothing
    }


{-| Specify an icon to use in the list item.

For more control over how to display the icon, use `iconConfig` instead.

-}
icon : Maybe Icon.Icon -> Config msg -> Config msg
icon i cfg =
    { cfg | icon = i }


{-| Specify an icon to use in the list item.
-}
iconConfig : Maybe (Icon.Config msg) -> Config msg -> Config msg
iconConfig iconCfg cfg =
    { cfg | iconCfg = iconCfg }


{-| Create a list item with the given properties and child elements.
-}
item : Config msg -> List (Html msg) -> ListItem msg
item =
    ListItem


{-| View a list item, using the specified HTML element constructor.
-}
toHtml : (List (Attribute msg) -> List (Html msg) -> Html msg) -> ListItem msg -> Html msg
toHtml node (ListItem cfg body) =
    node
        [ class "item" ]
        (case cfg.icon of
            Nothing ->
                body

            Just i ->
                let
                    iconCfg =
                        cfg.iconCfg |> Maybe.withDefault Icon.init
                in
                [ Icon.icon iconCfg i
                , div [ class "content" ] body
                ]
        )
