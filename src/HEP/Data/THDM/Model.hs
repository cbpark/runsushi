{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HEP.Data.THDM.Model
    (
      InputParam (..)
    , renderInputParam

    , THDMType (..)
    , fromIntToType

    , Angles
    , mkAngles
    , tanBeta
    , sin2Beta
    , cosBetaAlpha
    , sinBetaAlpha

    , ModelFiles
    , getFiles
    , getParam
    , getInputFile
    , getOutputFile

    , mkModelFiles
    ) where

import           Data.Double.Conversion.Text (toExponential, toFixed)
import           Data.Hashable               (Hashable, hash)
import           Data.Text                   (Text, replace)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Data.Text.Lazy.Builder      (Builder, fromText, singleton)
import           Pipes
import           System.FilePath             ((</>))

import           Control.Monad               (forever)
import           GHC.Generics                (Generic)
import           System.IO                   (IOMode (..), withFile)

data THDMType = TypeI | TypeII | UnknownType deriving (Eq, Generic)

instance Hashable THDMType

instance Show THDMType where
    show mdtyp | mdtyp == TypeI  = "1"
               | mdtyp == TypeII = "2"
               | otherwise       = "0"

fromIntToType :: Int -> THDMType
fromIntToType n | n == 1    = TypeI
                | n == 2    = TypeII
                | otherwise = UnknownType
{-# INLINE fromIntToType #-}

data InputParam = InputParam { _mdtyp :: !THDMType
                             , _mS    :: !Double
                             , _mH    :: !Double
                             , _mA    :: !Double
                             , _mHp   :: !Double
                             , _angs  :: !Angles
                             } deriving (Generic, Show)

instance Hashable InputParam

renderInputParam :: InputParam -> Builder
renderInputParam InputParam {..} =
    renderTHDMType _mdtyp
    <> space <> renderMass _mS
    <> space <> renderMass _mH
    <> space <> renderMass _mA
    <> space <> renderMass _mHp
    <> space <> renderAngles _angs

renderTHDMType :: THDMType -> Builder
renderTHDMType typ = let typ' | typ == TypeI  = '1'
                              | typ == TypeII = '2'
                              | otherwise     = '0'
                     in singleton typ'
{-# INLINE renderTHDMType #-}

renderMass :: Double -> Builder
renderMass = fromText . toFixed 2

newtype Angles = Angles (Double, Double) deriving (Show, Generic)

instance Hashable Angles

mkAngles :: Double -> Double -> Angles
mkAngles tanb cosba = Angles (tanb, cosba)
{-# INLINE mkAngles #-}

tanBeta :: Angles -> Double
tanBeta (Angles (tanb, _)) = tanb
{-# INLINE tanBeta #-}

sin2Beta :: Angles -> Double
sin2Beta (Angles (tanb, _)) = 2 * tanb / (1 + tanb * tanb)
{-# INLINE sin2Beta #-}

cosBetaAlpha :: Angles -> Double
cosBetaAlpha (Angles (_, cosba)) = cosba
{-# INLINE cosBetaAlpha #-}

sinBetaAlpha :: Angles -> Double
sinBetaAlpha (Angles (tanb, cosba)) = let b = atan tanb
                                          a = piHalf (b - acos cosba)
                                      in sin (b - a)
{-# INLINE sinBetaAlpha #-}

piHalf :: Double -> Double
piHalf th | th >=  pi12 = piHalf $! th - pi
          | th <  -pi12 = piHalf $! th + pi
          | otherwise   = th
  where pi12 = pi / 2

renderAngles :: Angles -> Builder
renderAngles angs =
    let tanb  = tanBeta angs
        cosba = cosBetaAlpha angs
    in (fromText . toFixed 1) tanb <> space <> (fromText . toFixed 2) cosba

space :: Builder
space = fromText " "

data ModelFiles = ModelFiles { _files :: [FilePath], _param :: InputParam }

getFiles :: ModelFiles -> [FilePath]
getFiles = _files

getParam :: ModelFiles -> InputParam
getParam = _param

getFile :: ([FilePath] -> FilePath) -> ModelFiles -> FilePath
getFile f (ModelFiles files _) = f files

getInputFile, getOutputFile :: ModelFiles -> FilePath
getInputFile  = getFile head
getOutputFile = getFile (head . tail)

mkModelFiles :: MonadIO m
             => Double    -- ^ sqrt(s)
             -> FilePath  -- ^ (temporary) work directory
             -> FilePath  -- ^ input template file
             -> Pipe InputParam ModelFiles m ()
mkModelFiles sqrtS workDir inpTmpF = forever $ do
    param@InputParam {..} <- await
    liftIO . putStrLn $ "---- m_H = " ++ show _mH ++ ", m_A = " ++ show _mA
    liftIO (mkModelFiles' sqrtS workDir inpTmpF param) >>= yield

mkModelFiles' :: MonadIO m
              => Double      -- ^ sqrt(s)
              -> FilePath    -- ^ (temporary) work directory
              -> FilePath    -- ^ input template file
              -> InputParam  -- ^ input parameters
              -> m ModelFiles
mkModelFiles' sqrtS workDir inpTmpF param = do
    let hashVal = show (hash param)
        inpF = workDir </> "input-" ++ hashVal ++ ".dat"

    inpStr <- mkInputFile' sqrtS inpTmpF param
    liftIO . withFile inpF WriteMode $ \h -> TIO.hPutStrLn h inpStr

    let outF = workDir </> "output-" ++ hashVal ++ ".dat"
    return $ ModelFiles { _files = [inpF, outF], _param = param }

mkInputFile' :: MonadIO m => Double -> FilePath -> InputParam -> m Text
mkInputFile' sqrtS inpTmpF InputParam {..} = do
    template <- T.lines <$> (liftIO . TIO.readFile) inpTmpF
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
