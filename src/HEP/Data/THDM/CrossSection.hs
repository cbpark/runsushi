{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HEP.Data.THDM.CrossSection (runSushi, getXSH2, printXS) where

import           HEP.Data.SLHA               (getSLHASpec, numValueOf)
import           HEP.Data.THDM.Model

import           Data.Double.Conversion.Text (toExponential, toFixed)
import           Data.Text.Lazy.Builder      (Builder, fromText, toLazyText)
import           Data.Text.Lazy.IO           (hPutStrLn)
import           Pipes
import qualified Pipes.Prelude               as P
import           System.Process              (readProcessWithExitCode)

import           Control.Monad               (forever)
import           System.IO                   (Handle)

runSushi :: MonadIO m => FilePath -> Pipe ModelFiles ModelFiles m ()
runSushi sushiexe = forever $ do
    modelFiles <- await
    let files = getFiles modelFiles
    liftIO (readProcessWithExitCode sushiexe files "") >> yield modelFiles

data XSH2 = XSH2 { _xs    :: !Double
                 , _xsGG  :: !Double
                 , _xsBB  :: !Double
                 , _sqrtS :: !Double
                 }

getXSH2 :: MonadIO m => Double -> Pipe ModelFiles Builder m ()
getXSH2 sqrtS = forever $ do
    modelFiles <- await
    lift (getXSH2' sqrtS modelFiles) >>= yield

getXSH2' :: MonadIO m => Double -> ModelFiles -> m Builder
getXSH2' sqrtS modelFiles = do
    let param = getParam modelFiles

    slha <- getSLHASpec (getOutputFile modelFiles)
    let xs = case slha of
                 Left  _      -> nullXSH2
                 Right blocks -> let xsGG = 1000 * numValueOf "SUSHIggh" 1 blocks
                                     xsBB = 1000 * numValueOf "SUSHIbbh" 1 blocks
                                 in XSH2 { _xs    = xsGG + xsBB
                                         , _xsGG  = xsGG
                                         , _xsBB  = xsBB
                                         , _sqrtS = sqrtS }
    return $ renderInputParam param <> space <> renderXSH2 xs
  where
    nullXSH2 = XSH2 { _xs = 0, _xsGG = 0, _xsBB = 0, _sqrtS = sqrtS }

renderXSH2 :: XSH2 -> Builder
renderXSH2 XSH2 {..} =
    (fromText . toFixed 1) _sqrtS
    <> space <> convXS _xs
    <> space <> convXS _xsGG
    <> space <> convXS _xsBB
  where convXS = fromText . toExponential 8

space :: Builder
space = fromText " "

printXS :: MonadIO m => Handle -> Consumer Builder m ()
printXS h = for (P.map toLazyText) (liftIO . hPutStrLn h)
