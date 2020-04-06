module HEP.Data.Util where

import Data.Vector      (Vector, generate)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath  ((</>))
import System.Random    (randomIO)

mkPoints :: Double -> [Double] -> (Vector Double, Int)
mkPoints stepsize vs =
    (generate npoints (\i -> vmin + fromIntegral i * stepsize), npoints)
    where (vmin, vmax) = (,) <$> minimum <*> maximum $ vs
          npoints = floor $ (vmax - vmin) / stepsize + 1
{-# INLINE mkPoints #-}

mkWorkDir :: IO FilePath
mkWorkDir = do r <- randomIO
               tmpDir <- getTemporaryDirectory
               let workDir = tmpDir </> ("runsushi" ++ take 8 (show (r :: Int)))
               createDirectoryIfMissing True workDir
               return workDir
{-# INLINE mkWorkDir #-}
