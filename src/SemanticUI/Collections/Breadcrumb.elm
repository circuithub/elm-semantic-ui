module SemanticUI.Collections.Breadcrumb exposing
  ( Config
  , BreadcrumbItem
  , init
  , size
  , divider
  , breadcrumb
  , breadcrumbPlain
  , active
  )
{- Provides [breadcrumb](https://semantic-ui.com/collections/breadcrumb.html) collection.

Usage example:

   import SemanticUI.Collections.Breadcrumb as Breadcrumb

   Breadcrumb.breadcrumbPlain
     Breadcrumb.init
     [ text "First", text "Second" ]
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import SemanticUI


{-| The configuration of a breadcrumb.
-}
type alias Config msg =
    { divider : Html msg
    , size : SemanticUI.Size
    }

{-| The simplest configuration of a breadcrumb. Corresponds to `class="ui breadcrumb"`.
-}
init : Config msg
init =
    { divider = text "/"
    , size = SemanticUI.Medium
    }

{-| Specify the size of a breadcrumb.
-}
size : SemanticUI.Size -> Config msg -> Config msg
size a model =
    { model | size = a }

{-| Specify the size of a breadcrumb.
-}
divider : Html msg -> Config msg -> Config msg
divider a model =
    { model | divider = a }

{-| A single breadcrumb with contents and possible "active" state
-}
type alias BreadcrumbItem msg =
    { active : Bool
    , content : Html msg
    }

active : Bool -> BreadcrumbItem msg -> BreadcrumbItem msg
active a model =
    { model | active = a }


{- view a breadcrumb -}
breadcrumb : Config msg -> List (BreadcrumbItem msg)  -> Html msg
breadcrumb cfg items = view cfg items

{- view a breadcrumb with chosen defaults (not active)-}
breadcrumbPlain : Config msg -> List (Html msg) -> Html msg
breadcrumbPlain cfg items = view cfg (List.map (\item -> BreadcrumbItem False item) items)

view : Config msg -> List (BreadcrumbItem msg) -> Html msg
view cfg breadcrumbs =
  let
    dividerEl = span [class "divider"] [ cfg.divider ]

    section item =
      div [ classList [("section", True), ("active", item.active)]
          ]
          [ item.content ]
  in
  div [ class "ui breadcrumb"
      , SemanticUI.sizeClass cfg.size
      ]
      (List.intersperse dividerEl (List.map section breadcrumbs))
