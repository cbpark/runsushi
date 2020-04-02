{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           HEP.Data.THDM
import           HEP.Data.Util    (mkPoints)

import qualified Data.Vector      as V
import           Options.Generic
import           System.Directory
import           System.FilePath  ((</>))
import           System.Process   (readProcess)

import           Control.Monad    (unless, when)
import           Data.Maybe       (fromMaybe)
import           System.Exit      (die)

main :: IO ()
main = do
    inp <- unwrapRecord "Run SuSHi to obtain the cross sections"

    let sushiexe = sushi inp
    putStrLn $ "-- We use SuSHi: " ++ sushiexe
    validExe <- isValidExecutable sushiexe
    unless validExe $ die ("-- Invalid SuSHi executable: " ++ sushiexe)

    let mdtyp = fromMaybe 2 (mtype inp)
        mdtypVal | mdtyp == 1 = TypeI
                 | mdtyp == 2 = TypeII
                 | otherwise  = UnknownType
    when (mdtypVal == UnknownType) $ die "The type must be either 1 or 2."

    let sqrtS = fromMaybe 13000 (eCM inp)
        step = fromMaybe 0.5 (stepsize inp)
        (mHVals, npoints) = mkPoints step (mH inp)
        mSVals = fromMaybe mHVals (V.replicateM npoints (mS inp))
        mHpVal = mHp inp
        mAVal = fromMaybe mHpVal (mA inp)
        (tanbVal, cosbaVal) = (,) <$> tanb <*> cosba $ inp

    putStrLn $ "-- m_{H+} = " ++ show mHpVal ++ ", tan(beta) = " ++ show tanbVal
        ++ ", cos(beta - alpha) = " ++ show cosbaVal

    let params = V.zipWith (\mHVal mSVal -> InputParam
                                            { _mdtyp = mdtypVal
                                            , _mS    = mSVal
                                            , _mH    = mHVal
                                            , _mA    = mAVal
                                            , _mHp   = mHpVal
                                            , _angs  = mkAngles tanbVal cosbaVal
                                            }) mHVals mSVals

    let inpTmpF = fromMaybe "input_template.in" (input inp)
    workDir <- (</> "runsushi") <$> getTemporaryDirectory
    createDirectoryIfMissing True workDir
    modelFiles <- V.mapM (mkModelFiles sqrtS workDir inpTmpF) params

    mapM_ (flip (readProcess sushiexe) []) (V.map getFiles modelFiles)
    getXSH2 sqrtS (V.head modelFiles) >>= print

    -- removeDirectoryRecursive workDir
    putStrLn "-- done."

isValidExecutable :: FilePath -> IO Bool
isValidExecutable exe = do
    exists <- doesFileExist exe
    isExec <- executable <$> getPermissions exe
    return $ exists && isExec

data InputArgs w = InputArgs
    { sushi    :: w ::: FilePath       <?> "SuSHi executable (which sushi)"
    , input    :: w ::: Maybe FilePath <?>
        "template for the input to SuSHi (default input_template.in)"
    , eCM      :: w ::: Maybe Double   <?>
        "center-of-mass energy in GeV (default: 13000 GeV)"
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
