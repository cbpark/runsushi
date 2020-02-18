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
    let str' = map (replaceSinBA
                    . replaceMCH
                    . replaceMA
                    . replaceMH
                    . replaceM12
                    . replaceTanb
                    . replaceTYPE
                    . replaceECM) str -- (take 25 str)
    mapM_ TIO.putStrLn str'
  where
    replaceECM   = T.replace "$ECM"     (toFixed 1 1.30e+4)
    replaceTYPE  = T.replace "$TYPE"    (T.pack (show (2 :: Int)))
    replaceTanb  = T.replace "$TANBETA" (toFixed 1 2)
    replaceM12   = T.replace "$M12"     (toExponential 7 50)
    replaceMH    = T.replace "$MH"      (toExponential 7 500)
    replaceMA    = T.replace "$MA"      (toExponential 7 500)
    replaceMCH   = T.replace "$MCH"     (toExponential 7 300)
    replaceSinBA = T.replace "$SINBA"   (toExponential 7 $ sqrt (1 - 0.1 ** 2))
