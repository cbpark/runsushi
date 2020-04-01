module Data.HEP.THDM.Model where

import Data.Text.Lazy.Builder (Builder, singleton)

data THDMType = TypeI | TypeII | UnknownType deriving (Eq, Show)

data InputParam = InputParam { _mdtyp :: THDMType
                             , _mS    :: Double
                             , _mH    :: Double
                             , _mA    :: Double
                             , _mHp   :: Double
                             , _angs  :: Angles
                             } deriving Show

renderTHDMType :: THDMType -> Builder
renderTHDMType typ = let typ' | typ == TypeI  = '1'
                              | typ == TypeII = '2'
                              | otherwise     = '0'
                     in singleton typ'

newtype Angles = Angles (Double, Double) deriving Show

mkAngles :: Double -> Double -> Angles
mkAngles tanb cosba = Angles (tanb, cosba)

tanBeta :: Angles -> Double
tanBeta (Angles (tanb, _)) = tanb

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
