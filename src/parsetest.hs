{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Attoparsec.Text
import           Data.Text            (Text)
-- import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO

import           Control.Monad        (void)
import           Prelude              hiding (takeWhile)
import           System.Environment   (getArgs)
import           System.IO            (IOMode (..), withFile)

main :: IO ()
main = do
    infile <- head <$> getArgs

    withFile infile ReadMode $ \h -> do
        contents <- TIO.hGetContents h
        case parseOnly slha contents of
            Left err     -> putStrLn err
            Right blocks -> mapM_ print blocks

newtype SLHAEntry = SLHAEntry (Int, Text) deriving Show

entry :: Parser SLHAEntry
entry = do
    skipComment >> skipSpace
    idx <- signed decimal <* skipSpace
    val <- textV
    return $ SLHAEntry (idx, val)

data SLHABlock = SLHABlock { blockName   :: Text
                           , slhaEntries :: [SLHAEntry]
                           } deriving Show

slhaBlock :: Parser SLHABlock
slhaBlock = do
    skipComment >> skipSpace
    name <- asciiCI "block" >> skipSpace *> textV
    skipComment >> skipSpace
    entries <- many' $ entry <* skipTillEnd
    return $ SLHABlock name entries

slha :: Parser [SLHABlock]
slha = many' slhaBlock

textV :: Parser Text
textV = takeWhile (\c -> c /= ' ' && c /= '#' && (not . isEndOfLine) c)

skipComment :: Parser ()
skipComment = void $ many' (char '#' >> skipTillEnd)

skipTillEnd :: Parser ()
skipTillEnd = skipWhile (not . isEndOfLine) >> endOfLine
