{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Double.Conversion.Text
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           System.Directory
import           System.Process              (readProcess)

import           Data.HEP.SLHA

import           Control.Monad               (unless)
import           System.Environment          (getArgs)
import           System.Exit                 (die)
import           System.IO                   (IOMode (..), withFile)

main :: IO ()
main = do
    [sushi, infile] <- getArgs
    putStrLn $ "-- We use SuSHi: " ++ sushi
    validExe <- isValidExecutable sushi
    unless validExe $ die ("-- Invalid SuSHi executable: " ++ sushi)

    str <- T.lines <$> TIO.readFile infile
    let str' = map (replaceSinBA
                    . replaceMCH
                    . replaceMA
                    . replaceMH
                    . replaceM12
                    . replaceTanb
                    . replaceTYPE
                    . replaceECM) str -- (take 25 str)

    -- tmpdir <- getTemporaryDirectory
    -- let inpF = tmpdir </> "input.dat"
    --     outF = tmpdir </> "output.dat"
    let inpF = "input.dat"
        outF = "output.dat"

    withFile inpF WriteMode $ \h -> mapM_ (TIO.hPutStrLn h) str'

    outputStr <- readProcess sushi [inpF, outF] []
    putStrLn outputStr

    slha <- getSLHASpec outF
    case slha of
        Left err     -> die err
        Right blocks -> do let xsGGH = numValueOf "SUSHIggh" 1 blocks
                               xsBBH = numValueOf "SUSHIbbh" 1 blocks
                               xs = xsGGH + xsBBH
                           print $ fmap (toExponential 8) [xs, xsGGH, xsBBH]

    -- putStrLn $ "-- The temporary files will be removed: "
    --     ++ inpF ++ ", " ++ outF
    -- mapM_ removeFile [inpF, outF]

  where
    replaceECM   = T.replace "$ECM"     (toFixed 1 1.30e+4)
    replaceTYPE  = T.replace "$TYPE"    (T.pack (show (2 :: Int)))
    replaceTanb  = T.replace "$TANBETA" (toFixed 1 2)
    replaceM12   = T.replace "$M12"     (toExponential 7 50)
    replaceMH    = T.replace "$MH"      (toExponential 7 500)
    replaceMA    = T.replace "$MA"      (toExponential 7 500)
    replaceMCH   = T.replace "$MCH"     (toExponential 7 300)
    replaceSinBA = T.replace "$SINBA"   (toExponential 7 $ sqrt (1 - 0.1 ** 2))

isValidExecutable :: FilePath -> IO Bool
isValidExecutable exe = do
    exists <- doesFileExist exe
    isExec <- executable <$> getPermissions exe
    return $ exists && isExec
