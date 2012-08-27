module Display
    (
      -- * Display type
      Display (..)

      -- * Window
    , Display.openWindow
    ) where

import Control.Arrow ((***))
import Control.Concurrent (forkIO)
import Control.Monad (when, unless)

import Data.IORef

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLFW as GLFW
import Graphics.Rendering.OpenGL (($=))

data Display = InWindow
    { title    :: String
    , position :: (Int, Int)
    , size     :: (Int, Int)
    } deriving (Eq)

openWindow :: Display -> IO ()
openWindow display = do
    GLFW.initialize
    let (width, height) = fromIntegral *** fromIntegral $ size display
    GLFW.openWindow
        (GL.Size width height) [GLFW.DisplayAlphaBits 8] GLFW.Window
    GLFW.windowTitle $= title display
    GL.shadeModel    $= GL.Smooth

    GL.lineSmooth $= GL.Enabled
    GL.blend      $= GL.Enabled
    GL.blendFunc  $= (GL.SrcAlpha, GL.OneMinusSrcAlpha)
    GL.lineWidth  $= 1.5

    GL.clearColor $= Color4 0 0 0 0

    GLFW.windowSizeCallback $= \size@(GL.Size w h) -> do
        GL.viewport   $= (GL.Position 0 0, size)
        GL.matrixMode $= GL.Projection
        GL.loadIdentity
        GL.ortho2D 0 (realToFrac w) (realToFrac h) 0

    quit <- newIORef False

    GLFW.disableSpecial GLFW.AutoPollEvent
    GLFW.windowCloseCallback $= (writeIORef quit True >> return True)

    let loop = do
        GLFW.waitEvents
        quit' <- readIORef quit
        unless quit' loop

    loop

    return ()
