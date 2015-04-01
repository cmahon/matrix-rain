{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}

module Grid where

import           Data.Tuple
import qualified Data.Vector as V
import           Linear

import           Types

-----------------------------------------------------------------------------

gridCells :: Grid -> Int
gridCells Grid{..} = _gridRows * _gridCols

gridPos :: Grid -> Int -> (Int,Int)
gridPos Grid{..} i = 
  let i' = mod i (_gridRows * _gridCols)
  in  if _gridRowMajor 
        then quotRem i' _gridCols
        else swap (quotRem i' _gridRows)

gridIndex :: Grid -> (Int,Int) -> Int
gridIndex Grid{..} (r,c) = r * rm + c * cm where
  (rm,cm) = if _gridRowMajor then (_gridCols,_gridRows) else (_gridRows,_gridCols)

cellSize :: Grid -> (Float,Float)
cellSize Grid{..} = (recip (fromIntegral _gridCols),recip (fromIntegral _gridRows))

cellVertices :: Grid -> Int -> V.Vector (V2 Float)
cellVertices g i = 
  let (xd,yd) = cellSize g
      (r,c) = gridPos g i
      x = (fromIntegral c * xd)
      y = (fromIntegral r * yd)
      x' = x + xd
      y' = y + yd
  in  [V2 x y', V2 x' y', V2 x' y, V2 x y]
