{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Ops
-- Copyright   :  (c) 2010-2011 Patrick Bahr, Tom Hvitved
-- License     :  BSD3
-- Maintainer  :  Patrick Bahr <paba@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module provides operators on functors.
--
--------------------------------------------------------------------------------

module Data.Comp.Ops where

import Data.Kind
import Data.Proxy

import Data.Comp.SubsumeCommon

infixr 6 :+:


-- |Formal sum of signatures (functors).
data (f :+: g) e = Inl (f e)
                 | Inr (g e)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (Inl e) = Inl (fmap f e)
    fmap f (Inr e) = Inr (fmap f e)

instance (Foldable f, Foldable g) => Foldable (f :+: g) where
    fold (Inl e) = fold e
    fold (Inr e) = fold e
    foldMap f (Inl e) = foldMap f e
    foldMap f (Inr e) = foldMap f e
    foldr f b (Inl e) = foldr f b e
    foldr f b (Inr e) = foldr f b e
    foldl f b (Inl e) = foldl f b e
    foldl f b (Inr e) = foldl f b e
    foldr1 f (Inl e) = foldr1 f e
    foldr1 f (Inr e) = foldr1 f e
    foldl1 f (Inl e) = foldl1 f e
    foldl1 f (Inr e) = foldl1 f e

instance (Traversable f, Traversable g) => Traversable (f :+: g) where
    traverse f (Inl e) = Inl <$> traverse f e
    traverse f (Inr e) = Inr <$> traverse f e
    sequenceA (Inl e) = Inl <$> sequenceA e
    sequenceA (Inr e) = Inr <$> sequenceA e

infixl 5 :<:

type family Elem (f :: Type -> Type) (g :: Type -> Type) :: Emb where
    Elem f f = Found Here
    Elem (f1 :+: f2) g =  Sum' (Elem f1 g) (Elem f2 g)
    Elem f (g1 :+: g2) = Choose (Elem f g1) (Elem f g2)
    Elem f g = NotFound

class Subsume (e :: Emb) (f :: Type -> Type) (g :: Type -> Type) where
  inj'  :: Proxy e -> f a -> g a
  prj'  :: Proxy e -> g a -> Maybe (f a)

instance Subsume (Found Here) f f where
    inj' _ = id

    prj' _ = Just

instance Subsume (Found p) f g => Subsume (Found (Le p)) f (g :+: g') where
    inj' _ = Inl . inj' (Proxy :: Proxy (Found p))

    prj' _ (Inl x) = prj' (Proxy :: Proxy (Found p)) x
    prj' _ _       = Nothing

instance Subsume (Found p) f g => Subsume (Found (Ri p)) f (g' :+: g) where
    inj' _ = Inr . inj' (Proxy :: Proxy (Found p))

    prj' _ (Inr x) = prj' (Proxy :: Proxy (Found p)) x
    prj' _ _       = Nothing

instance (Subsume (Found p1) f1 g, Subsume (Found p2) f2 g)
    => Subsume (Found (Sum p1 p2)) (f1 :+: f2) g where
    inj' _ (Inl x) = inj' (Proxy :: Proxy (Found p1)) x
    inj' _ (Inr x) = inj' (Proxy :: Proxy (Found p2)) x

    prj' _ x = case prj' (Proxy :: Proxy (Found p1)) x of
                 Just y -> Just (Inl y)
                 _      -> case prj' (Proxy :: Proxy (Found p2)) x of
                             Just y -> Just (Inr y)
                             _      -> Nothing



-- | A constraint @f :<: g@ expresses that the signature @f@ is
-- subsumed by @g@, i.e. @f@ can be used to construct elements in @g@.
type f :<: g = (Subsume (ComprEmb (Elem f g)) f g)

inj :: forall f g a . (f :<: g) => f a -> g a
inj = inj' (Proxy :: Proxy (ComprEmb (Elem f g)))

proj :: forall f g a . (f :<: g) => g a -> Maybe (f a)
proj = prj' (Proxy :: Proxy (ComprEmb (Elem f g)))
