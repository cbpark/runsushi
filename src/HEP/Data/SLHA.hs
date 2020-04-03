{-# LANGUAGE OverloadedStrings #-}

module HEP.Data.SLHA
    (
      SLHASpectrum (..)
    , slhaSpec
    , getSLHASpec
    , entryOf
    , numValueOf
    ) where

import           Data.Attoparsec.Text
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IntMap
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.IO           (hGetContents)
import qualified Data.Text.Read         as TR

import           Control.Monad          (void)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Maybe             (fromMaybe)
import           Prelude                hiding (takeWhile)
import           System.IO              (IOMode (..), withFile)

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
    return (T.toCaseFold blockName, IntMap.fromList entries)

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

getSLHASpec :: MonadIO m => FilePath -> m (Either String SLHASpectrum)
getSLHASpec fin =
    liftIO . withFile fin ReadMode $ \h -> do
        contents <- hGetContents h
        return (parseOnly slhaSpec contents)

entryOf :: Text -> Int -> SLHASpectrum -> Maybe Double
entryOf key i (SLHASpectrum blocks) = do
    block <- Map.lookup (T.toCaseFold key) blocks
    case IntMap.lookup i block of
        Just x  -> case TR.double x of
                       Left _                  -> Nothing
                       Right (num, unconsumed) -> if T.null unconsumed
                                                  then return num
                                                  else Nothing
        Nothing -> Nothing

numValueOf :: Text -> Int -> SLHASpectrum -> Double
numValueOf key i spec = fromMaybe 0 (entryOf key i spec)

textV :: Parser Text
textV = takeWhile (\c -> c /= ' ' && c /= '#' && (not . isEndOfLine) c)

skipComment :: Parser ()
skipComment = void $ many' (char '#' >> skipTillEnd)

skipTillEnd :: Parser ()
skipTillEnd = skipWhile (not . isEndOfLine) >> endOfLine
