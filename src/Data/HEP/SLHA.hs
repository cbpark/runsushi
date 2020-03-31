{-# LANGUAGE OverloadedStrings #-}

module Data.HEP.SLHA (SLHASpectrum (..), slhaSpec) where

import           Data.Attoparsec.Text
import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IntMap
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Text            (Text)

import           Control.Monad        (void)
import           Prelude              hiding (takeWhile)

newtype SLHASpectrum = SLHASpectrum (Map Text SLHAEntries) deriving Show

type SLHAEntries = IntMap Text

entry :: Parser (Int, Text)
entry = do
    skipComment >> skipSpace
    idx <- signed decimal <* skipSpace
    val <- textV
    return (idx, val)

slhaBlock :: Parser (Text, SLHAEntries)
slhaBlock = do
    skipComment >> skipSpace
    blockName <- asciiCI "block" >> skipSpace *> textV
    skipComment >> skipSpace
    entries <- many' $ entry <* skipTillEnd
    return (blockName, IntMap.fromAscList entries)

-- |
--
-- Example usage:
--
-- > import Data.Attoparsec.Text (parseOnly)
-- > import Data.Map             (toList)
-- > import Data.Text.IO         (hGetContents)
-- >
-- > import Data.HEP.SLHA
-- >
-- > import System.Environment   (getArgs)
-- > import System.IO            (IOMode (..), withFile)
-- >
-- > main :: IO ()
-- > main = do
-- >     infile <- head <$> getArgs
-- >     withFile infile ReadMode $ \h -> do
-- >         contents <- hGetContents h
-- >         case parseOnly slhaSpec contents of
-- >             Left err                    -> putStrLn err
-- >             Right (SLHASpectrum blocks) -> mapM_ print (toList blocks)
slhaSpec :: Parser SLHASpectrum
slhaSpec = SLHASpectrum . Map.fromAscList <$> many' slhaBlock

textV :: Parser Text
textV = takeWhile (\c -> c /= ' ' && c /= '#' && (not . isEndOfLine) c)

skipComment :: Parser ()
skipComment = void $ many' (char '#' >> skipTillEnd)

skipTillEnd :: Parser ()
skipTillEnd = skipWhile (not . isEndOfLine) >> endOfLine
