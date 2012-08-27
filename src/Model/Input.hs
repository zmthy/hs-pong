module Model.Input
    (
      -- * Input types
      Direction (..)
    , KeyInput (..)

      -- * Defaults
    , defaultKeyInput
    ) where


------------------------------------------------------------------------------
-- | The direction of paddle movement.
data Direction = Up | Neutral | Down
    deriving (Eq, Show)

------------------------------------------------------------------------------
-- | The possible state of all relevant keys.
data KeyInput = KeyInput Bool Direction Direction


------------------------------------------------------------------------------
-- | The default key state (entirely neutral).
defaultKeyInput :: KeyInput
defaultKeyInput = KeyInput False Neutral Neutral
