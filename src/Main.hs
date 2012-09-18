------------------------------------------------------------------------------
-- | The main module. Opens the window and binds the event network to it.
module Main where

------------------------------------------------------------------------------
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT

import Reactive.Banana
import Reactive.Banana.Frameworks

------------------------------------------------------------------------------
import Game.Graphics
import Game.Prelude

import Model


------------------------------------------------------------------------------
-- | Entry action.
main :: IO ()
main = compile networkDescription >>= actuate >> mainLoop


------------------------------------------------------------------------------
-- | The event network of the game.
networkDescription :: Frameworks t => Moment t ()
networkDescription = do
    ui <- setup display
    bind . stepper () $ () <$ ui
    return ()


------------------------------------------------------------------------------
-- | Window display information.
display :: Display
display = InWindow "Pong" (500, 500)


instance Render () where
    render () = clear [ColorBuffer] >> flush
