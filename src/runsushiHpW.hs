{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators      #-}

module Main where

import           HEP.Data.SUSHI.THDM
import           HEP.Data.SUSHI.Util

import qualified Data.Vector         as V
import           Options.Generic
import           Pipes
import qualified Pipes.Prelude       as P

import           Control.Monad       (when)
import           Data.Maybe          (fromMaybe)
import           System.Exit         (die)

main :: IO ()
main = do
    inp <- unwrapRecord "Run SusHi and h2decays to obtain the cross sections"

    let h2decaysExec = h2decays inp
        sushiExec = sushi inp
    putStrLn $ "-- We use h2decays from " ++ h2decaysExec
        ++ ", and SusHi from " ++ sushiExec

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
                                            , _mS    = Mass mSVal
                                            , _mH    = Mass mHVal
                                            , _mA    = Mass mAVal
                                            , _mHp   = Mass mHpVal
                                            , _angs  = mkAngles tanbVal cosbaVal
                                            }) mHVals mSVals

    runEffect $ each params >-> runh2decays h2decaysExec >-> P.print

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
    , mS       :: w ::: Maybe Double   <?> "heavy mass scale (m_A if MSSM)"
    , tanb     :: w ::: Double         <?> "tan(beta)"
    , cosba    :: w ::: Double         <?> "cos(beta-alpha)"
    , stepsize :: w ::: Maybe Double   <?> "step size (default: 0.5)"
    , output   :: w ::: Maybe String   <?> "the name of the output file"
    } deriving Generic

instance ParseRecord (InputArgs Wrapped)
deriving instance Show (InputArgs Unwrapped)

    -- let outputh2Decays = brh2 inp
    -- withFile outputh2Decays ReadMode $ \h ->
    --     runEffect $ (parseBRH2 . fromHandle) h >-> P.print
