module SemanticUI.Elements.Rail
    exposing
        ( leftRail
        , rightRail
        , Config
        , RailDistance(..)
        )

{-| A rail is used to show accompanying content outside the boundaries of the main view of a site.

# Viewing rails

@docs leftRail, rightRail

# Rail properties

@docs Config, RailDistance

-}

import Html exposing (..)
import Html.Attributes exposing (..)


{-| A rail can vary the distance between its main content.
-}
type RailDistance
    = Attached
    | Close
    | Loose


{-| The configuration of a rail.
-}
type alias Config =
    { dividing : Bool
    , distance : RailDistance
    }


rail : String -> Config -> List (Html msg) -> Html msg
rail side { dividing, distance } =
    div
        [ class "ui"
        , class side
        , class "rail"
        , classList [ ( "dividing", dividing ) ]
        , class <|
            case distance of
                Attached ->
                    "attached"

                Close ->
                    "close"

                Loose ->
                    ""
        ]


{-| Render a rail to the left side of some content.
-}
leftRail : Config -> List (Html msg) -> Html msg
leftRail =
    rail "left"


{-| Render a rail to the right side of some content.
-}
rightRail : Config -> List (Html msg) -> Html msg
rightRail =
    rail "right"
