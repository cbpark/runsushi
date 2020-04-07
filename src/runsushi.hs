{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           HEP.Data.SUSHI.THDM
import           HEP.Data.Util

import           Data.Text.Lazy      (Text, pack)
import           Data.Text.Lazy.IO   (hPutStrLn)
import qualified Data.Vector         as V
import           Options.Generic     hiding (Text)
import           Pipes
import           System.Directory

import           Control.Monad       (when)
import           Data.Maybe          (fromMaybe)
import           System.Exit         (die)
import           System.IO           (IOMode (..), withFile)

main :: IO ()
main = do
    inp <- unwrapRecord "Run SuSHi to obtain the cross sections"

    let sushiexe = sushi inp
    putStrLn $ "-- We use SuSHi: " ++ sushiexe

    let mdtypVal = fromIntToType $ fromMaybe 2 (mtype inp)
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
    workDir <- mkWorkDir
    putStrLn $ "-- The work directory is: " ++ workDir

    let outfile = fromMaybe "output_h2_xs.dat" (output inp)
    withFile outfile WriteMode $ \h -> do
        hPutStrLn h header
        runEffect $ each params
                    >-> mkModelFiles sqrtS workDir inpTmpF
                    >-> runSushi sushiexe
                    >-> getXSH2 sqrtS
                    >-> printXS h

    putStrLn $ "-- "  ++ workDir ++ " will be removed."
    removeDirectoryRecursive workDir
    putStrLn $ "-- Done. The output file is " ++ outfile ++ "."

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

header :: Text
header = pack $ "# " <>
         foldl1 (\v1 v2 -> v1 <> ", " <> v2)
         (zipWith (\n v -> "(" <> show n <> ") " <> v) ([1..] :: [Int])
          [ "type", "mS", "mH", "mA", "mHp", "tanb", "cosba"
          , "sqrt(s)", "sigma(pp) (fb)", "sigma(gg)", "sigma(bb)" ])
