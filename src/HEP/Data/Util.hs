module HEP.Data.Util where

import Data.Vector      (Vector, generate)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath  ((</>))

mkPoints :: Double -> [Double] -> (Vector Double, Int)
mkPoints stepsize vs =
    (generate npoints (\i -> vmin + fromIntegral i * stepsize), npoints)
    where (vmin, vmax) = (,) <$> minimum <*> maximum $ vs
          npoints = floor $ (vmax - vmin) / stepsize + 1

mkWorkDir :: IO FilePath
mkWorkDir = do workDir <- (</> "runsushi") <$> getTemporaryDirectory
               createDirectoryIfMissing True workDir
               return workDir
