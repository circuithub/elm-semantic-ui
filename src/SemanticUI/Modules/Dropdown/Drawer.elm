module SemanticUI.Modules.Dropdown.Drawer exposing
    ( State(..)
    , isToggled
    , isTransitioning
    , isVisible
    )

{-| The dropdown drawer
-}


{-| The current state of the dropdown drawer.
-}
type State
    = Closed
    | Opening
    | Opened
    | Closing


isToggled : State -> Bool
isToggled state =
    state == Opening || state == Opened


isTransitioning : State -> Bool
isTransitioning state =
    state == Opening || state == Closing


isVisible : State -> Bool
isVisible state =
    state /= Closed
