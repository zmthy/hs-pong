------------------------------------------------------------------------------
-- | Defines a typeclass for rendering data onto the screen, and a number of
-- helpers for instancing the class.
module Game.Graphics.Render
    (
      -- * The typeclass
      Render (..)
    ) where

------------------------------------------------------------------------------
import Game.Prelude


------------------------------------------------------------------------------
-- | Declares that its instances are capable of being rendered.
--
-- At the moment this just means that there is an associated IO action that
-- is assumed to perform the rendering action.
class Render a where
    render :: a -> IO ()
