{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Codec.Picture
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops       (whileM_)
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.State (StateT)
import           Data.Default
import           Data.Either.Combinators   (fromRight')
import           Data.Functor              ((<$>))
import           Data.Monoid
import qualified Data.Vector               as V
import           Data.Word
import           Graphics.GL.Low
import qualified Graphics.UI.GLFW          as GLFW
import           Linear
import           System.Random

import           Matrix
import           Mesh
import           Types
import           Util

import           Paths_matrix_rain

-----------------------------------------------------------------------------

main :: IO ()
main = do
  GLFW.init
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMajor 3)
  GLFW.windowHint (GLFW.WindowHint'ContextVersionMinor 3)
  GLFW.windowHint (GLFW.WindowHint'OpenGLForwardCompat True)
  GLFW.windowHint (GLFW.WindowHint'OpenGLProfile GLFW.OpenGLProfile'Core)
  GLFW.windowHint (GLFW.WindowHint'Samples 4)
  withWindow 640 480 "Matrix Rain" run

-----------------------------------------------------------------------------

setup :: IO (Program,Tex2D RGB)
setup = do
  vsource <- readFile =<< getDataFileName "vertShader.glsl"
  fsource <- readFile =<< getDataFileName "fragShader.glsl"
  prog <- newProgram vsource fsource
  ImageRGB8 (Image w h image) <- 
    ((return . fromRight') =<< readImage =<< getDataFileName "glyphs.png")
  texture <- newTexture2D image (Dimensions w h)
  setTex2DFiltering Linear
  return (prog, texture)

runFrame :: GLFW.Window -> MatrixT IO ()
runFrame win = do
  liftIO GLFW.pollEvents
  liftIO $ clearColorBuffer (0, 0, 0)
  draw
  liftIO $ GLFW.swapBuffers win
  update

run :: GLFW.Window -> IO ()
run win = do
  (prog, texture) <- setup
  let appcfg = AppConfig def prog texture
      appstate = def & asMatrixStrip .~ strip 
  void $ flip runReaderT appcfg $
    flip execStateT appstate $
      whileM_ (not `liftM` stop) $ 
        runFrame win
 where
  strip = def & msCells .~ V.replicate cells def
  cells = fromIntegral $ def ^. mcfNumCells
  stop = liftIO $ GLFW.windowShouldClose win
