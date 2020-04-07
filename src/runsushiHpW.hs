module Main where

import           HEP.Data.THDM.Parser (parseBRH2)

import           Pipes                (runEffect, (>->))
import           Pipes.ByteString     (fromHandle)
import qualified Pipes.Prelude        as P

import           System.Environment
import           System.IO

main :: IO ()
main = do
    infile <- head <$> getArgs
    withFile infile ReadMode $ \h ->
        runEffect $ (parseBRH2 . fromHandle) h >-> P.print
