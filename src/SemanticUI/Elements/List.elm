module SemanticUI.Elements.List exposing
    ( ul
    , Config, div, init
    , relaxed, divided, attributes, horizontal, bulleted, size
    )

{-| A list is used to group related content.

@docs ul, div
@docs Config, init
@docs relaxed, divided, attributes, horizontal, bulleted, size

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI exposing (..)
import SemanticUI.Elements.List.Item exposing (ListItem, toHtml)


{-| All properties of a list.
-}
type alias Config msg =
    { relaxed : Bool
    , divided : Bool
    , attributes : List (Attribute msg)
    , horizontal : Bool
    , bulleted : Bool
    , size : Size
    }


{-| A list with the simplest configuration.
-}
init : Config msg
init =
    { relaxed = False
    , divided = False
    , attributes = []
    , horizontal = False
    , bulleted = False
    , size = Medium
    }


{-| Specify whether to view the list items horizontally.
-}
horizontal : Bool -> Config msg -> Config msg
horizontal a config =
    { config | horizontal = a }


{-| Specify whether to mark each item with a bullet.
-}
bulleted : Bool -> Config msg -> Config msg
bulleted a config =
    { config | bulleted = a }


{-| Whether to relax the list's padding to provide more negative space.
-}
relaxed : Bool -> Config msg -> Config msg
relaxed a config =
    { config | relaxed = a }


{-| Whether to show dividers between the list items.
-}
divided : Bool -> Config msg -> Config msg
divided a config =
    { config | divided = a }


{-| Any other custom `Attribute`s to add to this button. Custom attributes
will be added before `elm-semantic-ui` attributes.
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes a config =
    { config | attributes = a }


{-| Specify the size of the list items.
-}
size : Size -> Config msg -> Config msg
size a config =
    { config | size = a }


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
        (List.map (toHtml listItemTag) items)
