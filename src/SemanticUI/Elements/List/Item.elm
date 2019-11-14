module SemanticUI.Elements.List.Item exposing
    ( Config
    , ListItem
    , icon
    , iconConfig
    , init
    , item
    , toHtml
    )

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (..)
import SemanticUI.Elements.Icon as Icon


type ListItem msg
    = ListItem (Config msg) (List (Html msg))


type alias Config msg =
    { icon : Maybe Icon.Icon
    , iconCfg : Maybe (Icon.Config msg)
    }


init : Config msg
init =
    { icon = Nothing
    , iconCfg = Nothing
    }


icon : Maybe Icon.Icon -> Config msg -> Config msg
icon i cfg =
    { cfg | icon = i }


iconConfig : Maybe (Icon.Config msg) -> Config msg -> Config msg
iconConfig iconCfg cfg =
    { cfg | iconCfg = iconCfg }


item : Config msg -> List (Html msg) -> ListItem msg
item =
    ListItem


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
