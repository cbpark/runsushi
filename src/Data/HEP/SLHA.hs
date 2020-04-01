{-# LANGUAGE OverloadedStrings #-}

module Data.HEP.SLHA (SLHASpectrum (..), slhaSpec, getSLHASpec) where

import           Data.Attoparsec.Text
import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IntMap
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Text            (Text, toCaseFold)
import           Data.Text.IO         (hGetContents)

import           Control.Monad        (void)
import           Prelude              hiding (takeWhile)
import           System.IO            (IOMode (..), withFile)

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
    return (toCaseFold blockName, IntMap.fromList entries)

-- |
--
-- Example usage:
--
-- > import Data.Attoparsec.Text (parseOnly)
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
-- >             Right (SLHASpectrum blocks) -> mapM_ print blocks
slhaSpec :: Parser SLHASpectrum
slhaSpec = SLHASpectrum . Map.fromList <$> many' slhaBlock

getSLHASpec :: FilePath -> IO (Either String SLHASpectrum)
getSLHASpec fin =
    withFile fin ReadMode $ \h -> do
        contents <- hGetContents h
        return (parseOnly slhaSpec contents)

textV :: Parser Text
textV = takeWhile (\c -> c /= ' ' && c /= '#' && (not . isEndOfLine) c)

skipComment :: Parser ()
skipComment = void $ many' (char '#' >> skipTillEnd)

skipTillEnd :: Parser ()
skipTillEnd = skipWhile (not . isEndOfLine) >> endOfLine
