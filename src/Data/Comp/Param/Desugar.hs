{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FlexibleInstances,
  UndecidableInstances, OverlappingInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Param.Desugar
-- Copyright   :  (c) 2011 Patrick Bahr, Tom Hvitved
-- License     :  BSD3
-- Maintainer  :  Tom Hvitved <hvitved@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This modules defines the 'Desugar' type class for desugaring of terms.
--
--------------------------------------------------------------------------------

module Data.Comp.Param.Desugar where

import Data.Comp.Param
import Data.Comp.Param.Derive

-- |The desugaring term homomorphism.
class (Difunctor f, Difunctor g) => Desugar f g where
    desugHom :: Hom f g
    desugHom = desugHom' . fmap Hole
    desugHom' :: f a (Cxt h g a b) -> Cxt h g a b
    desugHom' x = appCxt (desugHom x)

$(derive [liftSum] [''Desugar])

-- |Desugar a term.
desugar :: Desugar f g => Term f -> Term g
{-# INLINE desugar #-}
desugar = appHom desugHom

-- |Lift desugaring to annotated terms.
desugarA :: (Difunctor f', Difunctor g', DistAnn f p f', DistAnn g p g',
             Desugar f g) => Term f' -> Term g'
desugarA = appHom (propAnn desugHom)

-- |Default desugaring instance.
instance (Difunctor f, Difunctor g, f :<: g) => Desugar f g where
    desugHom = simpCxt . inj