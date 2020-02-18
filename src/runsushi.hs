{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Double.Conversion.Text
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO

import           System.Environment          (getArgs)

main :: IO ()
main = do
    infile <- head <$> getArgs
    str <- T.lines <$> TIO.readFile infile
    let str' = map (replaceTYPE . replaceECM) (take 15 str)
    mapM_ TIO.putStrLn str'
  where
    replaceECM  = T.replace "$ECM"  (toFixed 1 eCM)
    replaceTYPE = T.replace "$TYPE" (T.pack (show mType))

eCM :: Double
eCM = 1.30e+4

mType :: Int
mType = 2
