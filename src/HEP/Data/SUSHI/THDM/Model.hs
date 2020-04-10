{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HEP.Data.SUSHI.THDM.Model
    (
      module DK
    , module DT

    , renderInputParam

    , ModelFiles
    , getFiles
    , getParam
    , getInputFile
    , getOutputFile

    , mkModelFiles
    ) where

import           HEP.Data.SUSHI.Util

import           HEP.Data.Kinematics         as DK
import           HEP.Data.THDM               as DT

import           Data.Double.Conversion.Text (toExponential, toFixed)
import           Data.Hashable               (hash)
import           Data.Text                   (Text, replace)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO
import           Data.Text.Lazy.Builder      (Builder, fromText, singleton)
import           Pipes
import           System.Directory            (doesFileExist)
import           System.FilePath             ((</>))

import           Control.Monad               (forever)
import           System.IO

renderInputParam :: InputParam -> Builder
renderInputParam InputParam {..} =
    renderTHDMType _mdtyp
    <> space <> renderMass _mH
    <> space <> renderMass _mA
    <> space <> renderMass _mHp
    <> space <> renderMass _m12
    <> space <> renderAngles _angs

renderTHDMType :: THDMType -> Builder
renderTHDMType typ = let typ' | typ == TypeI  = '1'
                              | typ == TypeII = '2'
                              | otherwise     = '0'
                     in singleton typ'
{-# INLINE renderTHDMType #-}

renderMass :: Mass -> Builder
renderMass = fromText . toFixed 2 . getMass
{-# INLINE renderMass #-}

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

getFile :: MonadIO m => ([FilePath] -> FilePath) -> ModelFiles -> m (Maybe FilePath)
getFile select (ModelFiles files _) = do
    let f = select files
    exist <- liftIO $ doesFileExist f
    if exist
        then return (Just f)
        else do liftIO $ hPutStrLn stderr "---- error from SusHi!"
                return Nothing

getInputFile, getOutputFile :: MonadIO m => ModelFiles -> m (Maybe FilePath)
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
    replaceMH    = replace "$MH"       (toExponential 7 (getMass _mH))
    replaceMA    = replace "$MA"       (toExponential 7 (getMass _mA))
    replaceMCH   = replace "$MCH"      (toExponential 7 (getMass _mHp))
    replaceM12   = replace "$M12"      (toExponential 7 (getMass _m12))
    replaceTYPE  = replace "$TYPE"     ((T.pack . show) _mdtyp)
    replaceSinBA = let sinba = sinBetaAlpha _angs
                   in replace "$SINBA" (toExponential 7 sinba)
