{-# LANGUAGE GHC2021 #-}

module Data.Subtypes.Lens (_Subtype) where

import Control.Lens

import Data.Subtypes

_Subtype :: a :< b => Prism' b a
_Subtype = prism' inj proj
