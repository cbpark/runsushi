module Main where

import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO

import           System.Environment (getArgs)

main :: IO ()
main = do
    infile <- head <$> getArgs
    str <- TIO.readFile infile
    mapM_ TIO.putStrLn $ take 3 (T.lines str)
