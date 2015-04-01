{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Mesh where

import           Control.Lens
import qualified Data.Vector as V
import           Data.Word

import           Linear

-----------------------------------------------------------------------------

data MeshSpec = MeshSpec
  { _msPositions   ::  V.Vector (V3 Float)
  , _msColours     ::  V.Vector (V3 Float)
  , _msTexCoords   ::  V.Vector (V2 Float)
  , _msIndices     ::  V.Vector (V3 Word8)
  }

data MeshData = MeshData
  { _mdVertexData  :: V.Vector Float
  , _mdIndexData   :: V.Vector Word8
  }

makeLenses ''MeshSpec
makeLenses ''MeshData

-----------------------------------------------------------------------------

unpackIndices :: V.Vector (V3 Word8) -> V.Vector Word8
unpackIndices = V.concatMap $ \(V3 a b c) -> [a, b, c]

interleave :: V.Vector (V3 Float) -> V.Vector (V3 Float) -> V.Vector (V2 Float) 
              -> V.Vector Float
interleave positions colours texcoords = 
  V.foldr (V.++) V.empty $ V.zipWith3 combine positions colours texcoords
 where
  combine (V3 x y z) (V3 r g b) (V2 u v) = [x, y, z, r, g, b, u, v]

fromMeshSpec :: MeshSpec -> MeshData
fromMeshSpec MeshSpec{..} = MeshData  
 (interleave _msPositions _msColours _msTexCoords)
 (unpackIndices _msIndices)

