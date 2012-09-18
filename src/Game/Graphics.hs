------------------------------------------------------------------------------
-- | The core graphics module, which binds together GLFW and Reactive Banana.
module Game.Graphics
    (
      -- * Graphics functions
      setup
    , adjust
    , bind

    , module Game.Graphics.Render
    , module Game.Events.UIEvent

      -- * Display type
    , Display (..)

      -- * Event types
    , UIEvent (..)
    ) where

------------------------------------------------------------------------------
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Reactive.Banana
import Reactive.Banana.Frameworks

------------------------------------------------------------------------------
import Game.Events.UIEvent
import Game.Graphics.Render
import Game.Prelude


------------------------------------------------------------------------------
-- | Indicates how the graphics window should display.
data Display = InWindow
    { displayTitle :: String
    , displaySize  :: (Int, Int)
    } deriving (Eq, Show)


------------------------------------------------------------------------------
-- | Sets up and opens the display, and returns the UI events.
setup :: Frameworks t => Display -> Moment t (Event t UIEvent)
setup display = do
    liftIONow $ do
        _ <- getArgsAndInitialize
        adjust display
        createWindow $ displayTitle display
        unregisterDisplay

    onFrame <- fromAddHandler $ \cb -> do
        displayCallback $= cb Frame
        return unregisterDisplay
    onClose <- fromAddHandler $ \cb -> do
        -- closeCallback $= Just (cb Close)
        return $ closeCallback $= Nothing

    return $ onFrame `union` onClose
  where
    unregisterDisplay = displayCallback $= (clear [ColorBuffer] >> flush)


------------------------------------------------------------------------------
-- | Adjusts the display to new settings.
adjust :: Display -> IO ()
adjust display = do
    let (width, height) = displaySize display
    windowSize $= Size (fromIntegral width) (fromIntegral height)


------------------------------------------------------------------------------
-- | Binds the varying model to be rendered on the open display.
bind :: (Frameworks t, Render model) => Behavior t model -> Moment t ()
bind model = do
    first  <- initial model
    change <- changes model

    liftIONow $ render first
    reactimate $ render <$> change

    return ()
