{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HEP.Data.THDM.CrossSection where

-- import HEP.Data.SLHA               (SLHASpectrum, numValueOf)
-- import HEP.Data.THDM.Model         (InputParam, renderInputParam)

import Data.Double.Conversion.Text (toExponential, toFixed)
import Data.Text.Lazy.Builder      (Builder, fromText)

-- import Control.Monad.IO.Class      (MonadIO)

data XSH2 = XSH2 { _xs    :: Double
                 , _xsGG  :: Double
                 , _xsBB  :: Double
                 , _sqrtS :: Double
                 }

xsH2 :: Double -> Double -> Double -> XSH2
xsH2 xsGG xsBB sqrtS = XSH2 { _xs    = xsGG + xsBB
                            , _xsGG  = xsGG
                            , _xsBB  = xsBB
                            , _sqrtS = sqrtS }

renderXSH2 :: XSH2 -> Builder
renderXSH2 XSH2 {..} =
    (fromText . toFixed 1) _sqrtS
    <> space <> convXS _xs
    <> space <> convXS _xsGG
    <> space <> convXS _xsBB
  where convXS = fromText . toExponential 8

-- getXSH2 :: m Double -> InputParam -> SLHASpectrum -> m (Either String Builder)
-- getXSH2 sqrtS inp blocks = do
    -- let xsGG = numValueOf "SUSHIggh" 1 blocks
    --     xsBB = numValueOf "SUSHIbbh" 1 blocks
    --     xs = XSH2 { _xs    = xsGG + xsBB
    --               , _xsGG  = xsGG
    --               , _xsBB  = xsBB
    --               , _sqrtS = sqrtS }
    -- in renderInputParam inp <> space <> renderXSH2 xs

space :: Builder
space = fromText " "
