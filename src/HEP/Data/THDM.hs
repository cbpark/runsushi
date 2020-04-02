{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HEP.Data.THDM (module TC, module TM, mkInputFile) where

import           HEP.Data.THDM.CrossSection  as TC
import           HEP.Data.THDM.Model         as TM

import           Data.Double.Conversion.Text (toExponential, toFixed)
import           Data.Hashable               (hash)
import           Data.Text                   (Text, replace)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           System.FilePath             ((</>))

import           System.IO                   (IOMode (..), withFile)

mkInputFile :: Double -> FilePath -> FilePath -> InputParam -> IO (FilePath, Int)
mkInputFile sqrtS workDir inpTmpF inp = do
    let inpHash = hash inp
        inpF = workDir </> ("input-" ++ show inpHash ++ ".dat")

    putStrLn $ "-- The input file is: " ++ inpF

    inpStr <- mkInputFile' sqrtS inpTmpF inp
    withFile inpF WriteMode $ \h -> TIO.hPutStrLn h inpStr

    return (inpF, inpHash)

mkInputFile' :: Double -> FilePath -> InputParam -> IO Text
mkInputFile' sqrtS inpTmpF InputParam {..} = do
    template <- T.lines <$> TIO.readFile inpTmpF
    let inpTxt =   replaceECM
                 . replaceTanb
                 . replaceMH
                 . replaceMA
                 . replaceMCH
                 . replaceM12
                 . replaceTYPE
                 . replaceSinBA
                 <$> template
    return $ T.unlines inpTxt
  where
    replaceECM   = replace "$ECM"      (toFixed 1 sqrtS)
    replaceTanb  = replace "$TANBETA"  (toFixed 1 (tanBeta _angs))
    replaceMH    = replace "$MH"       (toExponential 7 _mH)
    replaceMA    = replace "$MA"       (toExponential 7 _mA)
    replaceMCH   = replace "$MCH"      (toExponential 7 _mHp)
    replaceM12   = let sin2b = sin2Beta _angs
                       m12 = sqrt $ 0.5 * _mS * _mS * sin2b
                   in replace "$M12"   (toExponential 7 m12)
    replaceTYPE  = replace "$TYPE"     ((T.pack . show) _mdtyp)
    replaceSinBA = let sinba = sinBetaAlpha _angs
                   in replace "$SINBA" (toExponential 7 sinba)
