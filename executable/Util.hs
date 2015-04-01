module Util where

import           Control.Monad
import           Graphics.GL.Low
import qualified Graphics.UI.GLFW        as GLFW

-----------------------------------------------------------------------------

withWindow :: Int -> Int -> String -> (GLFW.Window -> IO ()) -> IO ()
withWindow width height title f = do
  r <- GLFW.init
  when r $ do
    GLFW.setErrorCallback $ Just simpleErrorCallback  
    m <- GLFW.createWindow width height title Nothing Nothing
    case m of
      Nothing -> putStrLn "createWindow failed"
      Just win -> do
        GLFW.makeContextCurrent m
        (w,h) <- GLFW.getFramebufferSize win
        setViewport $ Viewport 0 0 w h
        GLFW.swapInterval 1
        f win
        GLFW.destroyWindow win
    GLFW.terminate
 where
  simpleErrorCallback e s = putStrLn $ unwords [show e, show s]
