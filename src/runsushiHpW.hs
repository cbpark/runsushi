{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import HEP.Data.THDM.Parser             (parseBRH2')

import Data.Attoparsec.ByteString.Char8 (parseOnly)
import Data.ByteString.Char8            (pack)
import Options.Generic
import System.Process

main :: IO ()
main = do
    inp <- unwrapRecord "Run SusHi and h2decays to obtain the cross sections"

    let h2decaysExec = h2decays inp
        -- sushiExec = sushi inp

    (_, str, _) <- readProcessWithExitCode h2decaysExec
                   [ "--mtype", "2"
                   , "--mH",    "800"
                   , "--mA",    "300"
                   , "--mHp",   "300"
                   , "--mS",    "800"
                   , "--tanb",  "3"
                   , "--cosba", "0.1"
                   ] ""
    case parseOnly parseBRH2' (pack str) of
        Left err -> print err
        Right r  -> print r

data InputArgs w = InputArgs
    { h2decays :: w ::: FilePath <?> "the executable path of h2decays"
    , sushi    :: w ::: FilePath <?> "the executable path of SusHi"
    } deriving Generic

instance ParseRecord (InputArgs Wrapped)
deriving instance Show (InputArgs Unwrapped)

    -- let outputh2Decays = brh2 inp
    -- withFile outputh2Decays ReadMode $ \h ->
    --     runEffect $ (parseBRH2 . fromHandle) h >-> P.print
