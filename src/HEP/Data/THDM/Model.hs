{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module HEP.Data.THDM.Model
    (
      InputParam (..)
    , renderInputParam

    , THDMType (..)
    , Angles
    , mkAngles
    , tanBeta
    , sin2Beta
    , cosBetaAlpha
    , sinBetaAlpha
    ) where

import Data.Double.Conversion.Text (toFixed)
import Data.Text.Lazy.Builder      (Builder, fromText, singleton)

data THDMType = TypeI | TypeII | UnknownType deriving Eq

instance Show THDMType where
    show mdtyp | mdtyp == TypeI  = "1"
               | mdtyp == TypeII = "2"
               | otherwise       = "0"

data InputParam = InputParam { _mdtyp :: THDMType
                             , _mS    :: Double
                             , _mH    :: Double
                             , _mA    :: Double
                             , _mHp   :: Double
                             , _angs  :: Angles
                             } deriving Show

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

renderMass :: Double -> Builder
renderMass = fromText . toFixed 2

newtype Angles = Angles (Double, Double) deriving Show

mkAngles :: Double -> Double -> Angles
mkAngles tanb cosba = Angles (tanb, cosba)

tanBeta :: Angles -> Double
tanBeta (Angles (tanb, _)) = tanb

sin2Beta :: Angles -> Double
sin2Beta (Angles (tanb, _)) = 2 * tanb / (1 + tanb * tanb)

cosBetaAlpha :: Angles -> Double
cosBetaAlpha (Angles (_, cosba)) = cosba

sinBetaAlpha :: Angles -> Double
sinBetaAlpha (Angles (tanb, cosba)) = let b = atan tanb
                                          a = piHalf (b - acos cosba)
                                      in sin (b - a)

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
