------------------------------------------------------------------------------
-- | Defines the possible UI events.
module Game.Events.UIEvent
    (
      -- * UI Event datatype
      UIEvent (..)
    ) where


------------------------------------------------------------------------------
-- | The possible events that can occur in the UI.
data UIEvent
    = Frame
    | Key -- Key KeyStatus
    | Mouse -- MouseButton MouseStatus
    | Close
    deriving (Eq, Show)
