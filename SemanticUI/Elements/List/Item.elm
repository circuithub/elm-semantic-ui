module SemanticUI.Elements.List.Item exposing
    ( Config
    , ListItem
    , icon
    , init
    , item
    , toHtml
    )

import Html exposing (Attribute, Html, div)
import Html.Attributes exposing (..)
import SemanticUI.Elements.Icon as Icon


type ListItem msg
    = ListItem Config (List (Html msg))


type alias Config =
    { icon : Maybe Icon.Icon
    }


init : Config
init =
    { icon = Nothing }


icon : Maybe Icon.Icon -> Config -> Config
icon i cfg =
    { cfg | icon = i }


item : Config -> List (Html msg) -> ListItem msg
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
                [ Icon.icon Icon.init i
                , div [ class "content" ] body
                ]
        )
