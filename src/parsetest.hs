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
    -- str <- T.lines <$> TIO.readFile infile
    -- mapM_ TIO.putStrLn str

    -- print $ parseOnly entry
    --         (T.pack "         1     7.01656687E-01   # ggh XS in pb\n")
    -- print $ parseOnly slhaBlock
    --         (T.pack "Block SUSHIggh # Bon appetit\n 1     7.01656687E-01   # ggh XS in pb\n       101     2.38358724E-04   # +/- integ. error: ggh XS in pb\n")
    -- print $ parseOnly slhaBlock
    --         (T.pack "Block SUSHIinfo\n 1   1.7.0        # SusHi version\n 5   1.7.0        # 2HDMC version\n")
    withFile infile ReadMode $ \h -> do
        contents <- TIO.hGetContents h
        mapM_ print (parseOnly slha contents)

newtype SLHAEntry = SLHAEntry (Int, Text) deriving Show

entry :: Parser SLHAEntry
entry = do
    skipComment >> skipSpace
    idx <- signed decimal <* skipSpace
    val <- takeWhile (\c -> c /= ' ' && c /= '#' && (not . isEndOfLine) c)
    return $ SLHAEntry (idx, val)

data SLHABlock = SLHABlock { blockName   :: Text
                           , slhaEntries :: [SLHAEntry]
                           } deriving Show

slhaBlock :: Parser SLHABlock
slhaBlock = do
    skipComment >> skipSpace
    name <- asciiCI "block" >> skipSpace
            *> takeWhile (\c -> c /= ' ' && c /= '#' && (not . isEndOfLine) c)
    skipComment >> skipSpace
    entries <- many' $ entry <* skipTillEnd
    return $ SLHABlock name entries

slha :: Parser [SLHABlock]
slha = many' slhaBlock

skipComment :: Parser ()
skipComment = void $ many' (char '#' >> skipTillEnd)

skipTillEnd :: Parser ()
skipTillEnd = skipWhile (not . isEndOfLine) >> endOfLine
