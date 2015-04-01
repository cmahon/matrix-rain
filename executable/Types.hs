{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Control.Lens
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Default
import qualified Data.Vector                as V
import           Graphics.GL.Low
import           Linear
import           System.Random

-----------------------------------------------------------------------------

data Grid = Grid 
  { _gridRows :: Int
  , _gridCols :: Int
  , _gridRowMajor :: Bool
  }

-----------------------------------------------------------------------------

data MatrixConfig = 
    MatrixConfig
      { _mcfCellSize :: Float
      , _mcfCellSpace :: Float
      , _mcfNumCells :: Int
      , _mcfMaxCellLife :: Int
      , _mcfGlyphGrid :: Grid
      , _mcfProbCursorChange :: Float
      , _mcfProbCursorLive :: Float
      , _mcfProbCursorStatic :: Float
      , _mcfProbCursorEmpty :: Float
      , _mcfFramesLiveChange :: Int
      , _mcfCursorSpeed :: Float
      , _mcfMinDepth :: Float
      , _mcfMaxDepth :: Float
      , _mcfMaxStrips :: Int
      , _mcfProbSpawn :: Float
      , _mcfDriftSpeed :: Float
      , _mcfMinStartY :: Bool
      , _mcfFogDensity :: Float
      , _mcfColourCycleSpeed :: Float
      , _mcfColourFallSpeed :: Float
      }

data MatrixCellState = 
    Empty
  | Static
  | Live 
      { _mcsCellLife :: Int
      }

data MatrixCell = 
    MatrixCell
      { _mclState :: MatrixCellState
      , _mclGlyph :: Int
      , _mclAge :: Int
      }

data MatrixCursor =
    MatrixCursor
      { _mcrState :: MatrixCellState
      , _mcrGlyph :: Int
      , _mcrPos :: Int
      , _mcrOffset :: Float
      , _mcrDrawing :: Bool
      }

data MatrixStrip =
    MatrixStrip
      { _msGen :: StdGen
      , _msFrame :: Int
      , _msPos :: V3 Float
      , _msCellSize :: Float
      , _msCellSpace :: Float
      , _msMaxCells :: Int
      , _msCells :: V.Vector MatrixCell
      , _msCursor :: MatrixCursor
      , _msMaxGlyphs :: Int
      , _msGlyphGrid :: Grid
      }

makeLenses ''MatrixConfig
makeLenses ''MatrixCellState
makeLenses ''MatrixCell
makeLenses ''MatrixCursor
makeLenses ''MatrixStrip

-----------------------------------------------------------------------------

instance Default MatrixConfig where
  def = MatrixConfig
      { _mcfCellSize = 0.05
      , _mcfCellSpace = 0.01
      , _mcfNumCells = 20
      , _mcfMaxCellLife = 120
      , _mcfGlyphGrid = Grid 8 8 False
      , _mcfProbCursorChange = 20
      , _mcfProbCursorLive = 30
      , _mcfProbCursorStatic = 30
      , _mcfProbCursorEmpty = 10
      , _mcfFramesLiveChange = 3
      , _mcfCursorSpeed = 0.1
      , _mcfMinDepth = 40
      , _mcfMaxDepth = 200
      , _mcfMaxStrips = 50
      , _mcfProbSpawn = 0.2
      , _mcfDriftSpeed = 0.4
      , _mcfMinStartY = True
      , _mcfFogDensity = 0.15
      , _mcfColourCycleSpeed = 0.05
      , _mcfColourFallSpeed = 0.02
      }

instance Default MatrixCell where
  def = MatrixCell
      { _mclState = Empty
      , _mclGlyph = 0
      , _mclAge = 0
      }

instance Default MatrixStrip where
  def = MatrixStrip 
      { _msGen = mkStdGen 1
      , _msFrame = 0
      , _msPos = V3 (-0.1) 0.8 0
      , _msCellSize = 0.05
      , _msCellSpace = 0.01
      , _msMaxCells = 25
      , _msCells = V.empty
      , _msCursor = MatrixCursor (Live 30) 1 1 1 True
      , _msMaxGlyphs = 64
      , _msGlyphGrid = Grid 8 8 False
      } 

-----------------------------------------------------------------------------

data AppConfig =
    AppConfig
      { _acMatrixConfig :: MatrixConfig
      , _acProgram :: Program
      , _acTexture :: Tex2D RGB
      }

data AppState =
    AppState
      { _asRndGen :: StdGen 
      , _asFrame :: Int
      , _asMatrixStrip :: MatrixStrip
      }

makeLenses ''AppConfig
makeLenses ''AppState

-----------------------------------------------------------------------------

instance Default AppState where
  def = AppState (mkStdGen 1) 0 def

-----------------------------------------------------------------------------

type MatrixT m a = 
  (Functor m,Monad m) => StateT AppState (ReaderT AppConfig m) a

type StateMatrixT s m a = 
  (Functor m,Monad m) => StateT s (StateT AppState (ReaderT AppConfig m)) a


