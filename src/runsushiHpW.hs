{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           HEP.Data.SUSHI.THDM
import           HEP.Data.SUSHI.Util

import           Data.Text.Lazy.IO   (hPutStrLn)
import qualified Data.Vector         as V
import           Options.Generic
import           Pipes
import           Pipes.Lift          (runReaderP)
import           System.Directory    (removeDirectoryRecursive)

import           Control.Monad       (when)
import           Data.Maybe          (fromMaybe)
import           System.Exit         (die)
import           System.IO           (IOMode (..), stdout, withFile)

main :: IO ()
main = do
    inp <- unwrapRecord
           "Run SusHi and h2decays to obtain the cross sections (H+ W-)"

    let h2decaysExec = h2decays inp
        sushiExec = sushi inp
    putStrLn $ "-- We use h2decays from " ++ h2decaysExec
        ++ ", and SusHi from " ++ sushiExec

    let mdtypVal = fromIntToType $ fromMaybe 2 (mtype inp)
    when (mdtypVal == UnknownType) $ die "-- The type must be either 1 or 2."

    let sqrtS = fromMaybe 13000 (eCM inp)
        step = fromMaybe 0.5 (stepsize inp)
        (tanbVal, cosbaVal) = (,) <$> tanb <*> cosba $ inp
        (mHVals, npoints) = mkPoints step (mH inp)
        mHpVal = mHp inp
        mAVal = fromMaybe mHpVal (mA inp)
        m12Vals = fromMaybe (defaultM12 tanbVal <$> mHVals)
                  (V.replicateM npoints (m12 inp))

    putStrLn $ "-- m_{H+} = " ++ show mHpVal ++ ", tan(beta) = " ++ show tanbVal
        ++ ", cos(beta - alpha) = " ++ show cosbaVal

    let params = V.zipWith (\mHVal m12Val -> InputParam
                                              { _mdtyp = mdtypVal
                                              , _mH    = Mass mHVal
                                              , _mA    = Mass mAVal
                                              , _mHp   = Mass mHpVal
                                              , _m12   = Mass m12Val
                                              , _angs  = mkAngles tanbVal cosbaVal
                                              }) mHVals m12Vals

    let inpTmpF = fromMaybe "input_template.in" (input inp)
    workDir <- mkWorkDir
    putStrLn $ "-- The work directory is: " ++ workDir

    let writeOutput h =
            V.mapM_
            (\p -> runEffect $
                runReaderP p (runh2decays h2decaysExec
                              >-> getBRHpW
                              >-> runSushiWithBR sqrtS workDir inpTmpF sushiExec)
                >-> printXS h) params

    case output inp of
        Nothing      -> writeOutput stdout
        Just outfile -> do withFile outfile WriteMode $ \h -> do
                               hPutStrLn h ("# pp --> H --> H^\\pm W^\\mp\n" <> header)
                               writeOutput h
                           putStrLn $ "-- " ++ outfile ++ " generated."

    putStrLn $ "-- "  ++ workDir ++ " will be removed."
    removeDirectoryRecursive workDir
    putStrLn "-- Done!"

data InputArgs w = InputArgs
    { h2decays :: w ::: FilePath       <?> "the executable path of h2decays"
    , sushi    :: w ::: FilePath       <?> "the executable path of SusHi"
    , input    :: w ::: Maybe FilePath <?>
        "template for the input to SuSHi (default input_template.in)"
    , eCM      :: w ::: Maybe Double   <?>
        "center-of-mass energy in GeV (default: 13000 GeV)"
    , mtype    :: w ::: Maybe Int      <?> "model type (either 1 or 2)"
    , mH       :: w ::: [Double]       <?> "heavy Higgs mass"
    , mA       :: w ::: Maybe Double   <?> "CP-odd Higgs mass"
    , mHp      :: w ::: Double         <?> "charged Higgs mass"
    , m12      :: w ::: Maybe Double   <?> "soft Z2 breaking term"
    , tanb     :: w ::: Double         <?> "tan(beta)"
    , cosba    :: w ::: Double         <?> "cos(beta-alpha)"
    , stepsize :: w ::: Maybe Double   <?> "step size (default: 0.5)"
    , output   :: w ::: Maybe String   <?> "the name of the output file"
    } deriving Generic

instance ParseRecord (InputArgs Wrapped)
deriving instance Show (InputArgs Unwrapped)
