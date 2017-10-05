module SemanticUI.Views.Cards.Card exposing (..)

{-| View a single card.

# Viewing cards

@docs linkCard

# Card properties

@docs init, Config, attributes

## Fluid

A fluid card takes up the width of its container.

@docs fluid

-}

import Html exposing (..)
import Html.Attributes exposing (..)


{-| The configuration of a card.
-}
type alias Config msg =
    { fluid : Bool
    , attributes : List (Attribute msg)
    }


{-| Specify whether or not a card is fluid.
-}
fluid : Bool -> Config msg -> Config msg
fluid fluid model =
    { model | fluid = fluid }


{-| Custom attributes for a card.
-}
attributes : List (Attribute msg) -> Config msg -> Config msg
attributes attrs model =
    { model | attributes = attrs }


{-| The simplest configuration of a card. Corresponds to `class="ui card"`.
-}
init : Config msg
init =
    { fluid = False
    , attributes = []
    }


{-| View a card as a link. Cards can contain a variety of content,
which can be supplied when viewing the card. An empty lists will be
omitted entirely.
-}
linkCard :
    Config msg
    -> { image : Maybe String
       , content : List (Html msg)
       , extraContent : List (Html msg)
       }
    -> Html msg
linkCard =
    viewCard a


divCard :
    Config msg
    -> { image : Maybe String
       , content : List (Html msg)
       , extraContent : List (Html msg)
       }
    -> Html msg
divCard =
    viewCard div


viewCard el { fluid, attributes } { image, content, extraContent } =
    el
        (List.concat
            [ [ class "ui card"
              , classList [ ( "fluid", fluid ) ]
              ]
            , attributes
            ]
        )
        (List.concat
            [ [ div [ class "image" ]
                    (case image of
                        Nothing ->
                            []

                        Just image ->
                            [ img [ src image ] [] ]
                    )
              ]
            , case content of
                [] ->
                    []

                _ ->
                    [ div [ class "content" ] content ]
            , case extraContent of
                [] ->
                    []

                _ ->
                    [ div [ class "extra content" ] extraContent ]
            ]
        )
