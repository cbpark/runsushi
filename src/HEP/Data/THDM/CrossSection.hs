{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HEP.Data.THDM.CrossSection (getXSH2) where

import HEP.Data.SLHA               (getSLHASpec, numValueOf)
import HEP.Data.THDM.Model

import Data.Double.Conversion.Text (toExponential, toFixed)
import Data.Text.Lazy              (Text)
import Data.Text.Lazy.Builder      (Builder, fromText, toLazyText)

data XSH2 = XSH2 { _xs    :: Double
                 , _xsGG  :: Double
                 , _xsBB  :: Double
                 , _sqrtS :: Double
                 }

renderXSH2 :: XSH2 -> Builder
renderXSH2 XSH2 {..} =
    (fromText . toFixed 1) _sqrtS
    <> space <> convXS _xs
    <> space <> convXS _xsGG
    <> space <> convXS _xsBB
  where convXS = fromText . toExponential 8

getXSH2 :: Double -> ModelFiles -> IO Text
getXSH2 sqrtS modelFiles = do
    let param = getParam modelFiles

    slha <- getSLHASpec (getOutputFile modelFiles)
    let xs = case slha of
                 Left  _      -> nullXSH2
                 Right blocks -> let xsGG = numValueOf "SUSHIggh" 1 blocks
                                     xsBB = numValueOf "SUSHIbbh" 1 blocks
                                 in XSH2 { _xs    = xsGG + xsBB
                                         , _xsGG  = xsGG
                                         , _xsBB  = xsBB
                                         , _sqrtS = sqrtS }
    return $ toLazyText $ renderInputParam param <> space <> renderXSH2 xs
  where
    nullXSH2 = XSH2 { _xs = 0, _xsGG = 0, _xsBB = 0, _sqrtS = sqrtS }

space :: Builder
space = fromText " "
