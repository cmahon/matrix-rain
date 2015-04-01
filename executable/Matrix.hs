{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Matrix where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader (ReaderT)
import           Control.Monad.Trans.State (StateT)
import           Data.Default
import           Data.Monoid
import           Data.Tuple
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import           Data.Word
import           Graphics.GL.Low
import           Linear
import           System.Random

import           Grid
import           Mesh
import           Types
import           Util

-----------------------------------------------------------------------------

toMeshSpec :: MatrixConfig -> MatrixStrip -> MeshSpec
toMeshSpec MatrixConfig{..} MatrixStrip{..} = 
  MeshSpec positions colours texcoords indices
 where
  MatrixCursor{..} = _msCursor
  numCells = V.length _msCells
  quadVertices = [V2 0 0, V2 1 0, V2 1 1, V2 0 1]
  quadVertices' = V.map (\(V2 x y) -> V3 x y 0) quadVertices 
  yoffset n = negate (fromIntegral n * (_msCellSize + _msCellSpace))
  position j n = V.zipWith (^+^)
    (V.replicate 4 (_msPos ^+^ V3 0 (yoffset n + j) 0))
    (V.map (_msCellSize *^) quadVertices')
  positions = 
    V.concatMap (position 0) [1..numCells] <>
    position (_mcrOffset * _msCellSize) _mcrPos 
  colours = 
    V.replicate (numCells*4) (V3 0 1 0) <>
    V.replicate 4 (V3 1 1 1)
  texcoords = 
     V.concatMap (cellVertices _msGlyphGrid . _mclGlyph) _msCells <>
     cellVertices _msGlyphGrid (_msCursor ^. mcrGlyph)
  indices = quads . V.zipWith forFace [0..] $ 
              V.replicate (numCells+1) (0, 1, 2, 3)
  forFace i (a, b, c, d) = (a + i*4, b + i*4, c + i*4, d + i*4)
  quads = V.concatMap triangulate
  triangulate (a, b, c, d) = [V3 a b c, V3 c d a]

-----------------------------------------------------------------------------

draw :: MatrixT IO ()
draw = do
  ms <- use asMatrixStrip
  mc <- view acMatrixConfig
  prg <- view acProgram
  tex <- view acTexture
  let MeshData{..} = fromMeshSpec . toMeshSpec mc $ ms
      numIndices = V.length _mdIndexData
  liftIO $ do
    vao <- newVAO
    bindVAO vao
    useProgram prg
    vbo <- newVBO (VS.convert _mdVertexData) StaticDraw
    bindVBO vbo
    setVertexLayout 
      [ Attrib "position" 3 GLFloat
      , Attrib "colour" 3 GLFloat
      , Attrib "texcoord" 2 GLFloat
      ]
    indices <- newElementArray (VS.convert _mdIndexData) StaticDraw
    bindElementArray indices
    bindTexture2D tex
    drawIndexedTriangles numIndices UByteIndices

update :: MatrixT m ()
update = do 
  MatrixConfig{..} <- view acMatrixConfig
  f <- asFrame <+= 1
  zoom asMatrixStrip $ do
    msCursor . mcrOffset -= 0.1
    MatrixCursor{..} <- use msCursor
    when (_mcrOffset < 0) $ do
      msCursor . mcrOffset .= 1
      if _mcrDrawing
        then do 
          (msCells . ix (_mcrPos-1)) %= (mclState .~ _mcrState) . 
                                    (mclGlyph .~ _mcrGlyph)
        else do
          (msCells . ix (_mcrPos-1)) %= (mclState .~ Empty) .
                                    (mclGlyph .~ 0)
      cp <- msCursor . mcrPos <+= 1
      when (cp > _mcfNumCells) $ do
        cd <- use (msCursor . mcrDrawing)
        (msCursor . mcrDrawing) %= not
        (msCursor . mcrPos) .= 1
  when (mod f _mcfFramesLiveChange == 0) $ do
    randomiseCursorGlyph
    cells <- use (asMatrixStrip . msCells)
    cells' <- flip evalStateT Nothing (traverse updateCell cells)
    (asMatrixStrip . msCells) .= cells'

rndR :: (Int,Int) -> MatrixT m Int
rndR (l,h) = do
  g <- use asRndGen
  let (i,g') = randomR (l,h) g
  asRndGen .= g'
  return i

rndGlyph :: MatrixT m Int
rndGlyph = do
  n <- gridCells `liftM` view (acMatrixConfig . mcfGlyphGrid)
  rndR (0,n-1)

randomiseCursorGlyph :: MatrixT m ()
randomiseCursorGlyph = do
  i <- rndGlyph
  (asMatrixStrip . msCursor . mcrGlyph) .= i

updateCell :: (Functor m, Monad m) => 
              MatrixCell -> 
              StateMatrixT (Maybe MatrixCell) m MatrixCell
updateCell cell@MatrixCell{..} = do
  mpcell <- get
  cell' <- case _mclState of 
    Live l ->    do glyph <- case mpcell of 
                      Nothing -> lift rndGlyph
                      Just pcell -> return (pcell ^. mclGlyph)
                    return $ cell & mclGlyph .~ glyph
    otherwise -> return cell
  put $ Just cell
  return cell'

