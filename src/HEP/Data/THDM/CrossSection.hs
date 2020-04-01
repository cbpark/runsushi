{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HEP.Data.THDM.CrossSection where

import Data.Double.Conversion.Text (toExponential, toFixed)
import Data.Text.Lazy.Builder      (Builder, fromText)

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
  where
    convXS = fromText . toExponential 8
    space = fromText " "
