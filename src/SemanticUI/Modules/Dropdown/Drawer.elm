module SemanticUI.Modules.Dropdown.Drawer exposing (State(..), isToggled, isTransitioning, isVisible)

{-| The dropdown drawer.

@docs State, isToggled, isTransitioning, isVisible

-}


{-| The current state of the dropdown drawer.
-}
type State
    = Closed
    | Opening
    | Opened
    | Closing


{-| Check whether the dropdown is opening or opened.
-}
isToggled : State -> Bool
isToggled state =
    state == Opening || state == Opened


{-| Check whether the dropdown is in the middle of animating open or closed.
-}
isTransitioning : State -> Bool
isTransitioning state =
    state == Opening || state == Closing


{-| Check whether the dropdown is not closed.
-}
isVisible : State -> Bool
isVisible state =
    state /= Closed
