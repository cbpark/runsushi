module HEP.Data.SUSHI.Util (module DU, mkWorkDir) where

import HEP.Data.Util    as DU

import System.Directory (createDirectoryIfMissing, getTemporaryDirectory)
import System.FilePath  ((</>))
import System.Random    (randomRIO)

mkWorkDir :: IO FilePath
mkWorkDir = do r <- randomRIO (10000, 99999)
               tmpDir <- getTemporaryDirectory
               let workDir = tmpDir </> ("runsushi" ++ show (r :: Int))
               createDirectoryIfMissing True workDir
               return workDir
{-# INLINE mkWorkDir #-}
