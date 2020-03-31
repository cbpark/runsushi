module Main where

import Data.Attoparsec.Text (parseOnly)
import Data.Map             (toList)
import Data.Text.IO         (hGetContents)

import Data.HEP.SLHA

import System.Environment   (getArgs)
import System.IO            (IOMode (..), withFile)

main :: IO ()
main = do
    infile <- head <$> getArgs
    withFile infile ReadMode $ \h -> do
        contents <- hGetContents h
        case parseOnly slhaSpec contents of
            Left err                    -> putStrLn err
            Right (SLHASpectrum blocks) -> mapM_ print (toList blocks)
