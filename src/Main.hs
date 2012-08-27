module Main where

------------------------------------------------------------------------------
import Display

import Model


------------------------------------------------------------------------------
-- | Entry action.
main :: IO ()
main = do
    Display.openWindow (InWindow "Title" (50, 50) (500, 500))
