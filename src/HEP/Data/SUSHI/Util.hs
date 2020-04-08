module HEP.Data.SUSHI.Util (module DU, mkWorkDir, header) where

import HEP.Data.Util    as DU

import Data.Text.Lazy   (Text, pack)
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

header :: Text
header = pack $ "# " <>
         foldl1 (\v1 v2 -> v1 <> ", " <> v2)
         (zipWith (\n v -> "(" <> show n <> ") " <> v) ([1..] :: [Int])
          [ "type", "mS", "mH", "mA", "mHp", "tanb", "cosba"
          , "sqrt(s)", "sigma(pp) (fb)", "sigma(gg)", "sigma(bb)" ])
