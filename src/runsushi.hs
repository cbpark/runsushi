{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           Data.HEP.SLHA

import           Data.Double.Conversion.Text
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Options.Generic
import           System.Directory
import           System.Process              (readProcess)

import           Control.Monad               (unless)
-- import           Data.Maybe                  (fromMaybe)
import           System.Environment          (getArgs)
import           System.Exit                 (die)
import           System.IO                   (IOMode (..), withFile)

main :: IO ()
main = do
    -- inp <- unwrapRecord "Run SuSHi to obtain the cross sections"

    -- let mdtyp = fromMaybe 2 (mtype inp)

    [sushiPath, infile] <- getArgs
    putStrLn $ "-- We use SuSHi: " ++ sushiPath
    validExe <- isValidExecutable sushiPath
    unless validExe $ die ("-- Invalid SuSHi executable: " ++ sushiPath)

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

    outputStr <- readProcess sushiPath [inpF, outF] []
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

data InputArgs w = InputArgs
    { sushi    :: w ::: FilePath       <?> "SuSHi executable (which sushi)"
    , input    :: w ::: Maybe FilePath <?> "template for the input to SuSHi (input_template.in)"
    , mtype    :: w ::: Maybe Int      <?> "model type (either 1 or 2)"
    , mH       :: w ::: [Double]       <?> "heavy Higgs mass"
    , mA       :: w ::: Maybe Double   <?> "CP-odd Higgs mass"
    , mHp      :: w ::: Double         <?> "charged Higgs mass"
    , mS       :: w ::: Maybe Double   <?> "heavy mass scale (m_A if MSSM)"
    , tanb     :: w ::: Double         <?> "tan(beta)"
    , cosba    :: w ::: Double         <?> "cos(beta-alpha)"
    , stepsize :: w ::: Maybe Double   <?> "step size (default: 0.5)"
    , output   :: w ::: Maybe String   <?> "the name of the output file"
    } deriving Generic

instance ParseRecord (InputArgs Wrapped)
deriving instance Show (InputArgs Unwrapped)
