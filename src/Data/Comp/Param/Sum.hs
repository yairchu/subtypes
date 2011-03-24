{-# LANGUAGE TypeOperators, MultiParamTypeClasses, IncoherentInstances,
  FlexibleInstances, FlexibleContexts, GADTs, TypeSynonymInstances,
  ScopedTypeVariables #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Comp.Param.Sum
-- Copyright   :  (c) 2010-2011 Patrick Bahr, Tom Hvitved
-- License     :  BSD3
-- Maintainer  :  Tom Hvitved <hvitved@diku.dk>
-- Stability   :  experimental
-- Portability :  non-portable (GHC Extensions)
--
-- This module provides the infrastructure to extend signatures.
--
--------------------------------------------------------------------------------

module Data.Comp.Param.Sum
    (
     (:<:)(..),
     (:+:)(..),

     -- * Projections for Signatures and Terms
     proj2,
     proj3,
     project,
     project2,
     project3,
     deepProject,
     deepProject2,
     deepProject3,
     deepProject',
     deepProject2',
     deepProject3',

     -- * Injections for Signatures and Terms
     inj2,
     inj3,
     inject,
     inject2,
     inject3,
     deepInject,
     deepInject2,
     deepInject3,

     -- * Injections and Projections for Constants
     injectConst,
     injectConst2,
     injectConst3,
     projectConst,
     injectCxt,
     liftCxt,
     substHoles,
     substHoles'
    ) where

import Prelude hiding (sequence)
import Control.Monad hiding (sequence)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Comp.Param.Term
import Data.Comp.Param.Algebra
import Data.Comp.Param.Ops
import Data.Comp.Param.Functor
import Data.Comp.Param.Traversable




{-| A variant of 'proj' for binary sum signatures.  -}
proj2 :: forall f g1 g2 a e. (g1 :<: f, g2 :<: f) => f a e
      -> Maybe ((g1 :+: g2) a e)
proj2 x = case proj x of
            Just (y :: g1 a e) -> Just $ inj y
            _ -> liftM inj (proj x :: Maybe (g2 a e))

{-| A variant of 'proj' for ternary sum signatures.  -}
proj3 :: forall f g1 g2 g3 a e. (g1 :<: f, g2 :<: f, g3 :<: f) => f a e
      -> Maybe ((g1 :+: g2 :+: g3) a e)
proj3 x = case proj x of
            Just (y :: g1 a e) -> Just $ inj y
            _ -> case proj x of
                   Just (y :: g2 a e) -> Just $ inj y
                   _ -> liftM inj (proj x :: Maybe (g3 a e))

-- |Project the outermost layer of a term to a sub signature.
project :: (g :<: f) => Cxt h f p a -> Maybe (g p (Cxt h f p a))
project (Hole _) = Nothing
project (Term t) = proj t

-- |Project the outermost layer of a term to a binary sub signature.
project2 :: (g1 :<: f, g2 :<: f) => Cxt h f p a
         -> Maybe ((g1 :+: g2) p (Cxt h f p a))
project2 (Hole _) = Nothing
project2 (Term t) = proj2 t

-- |Project the outermost layer of a term to a ternary sub signature.
project3 :: (g1 :<: f, g2 :<: f, g3 :<: f) => Cxt h f p a
         -> Maybe ((g1 :+: g2 :+: g3) p (Cxt h f p a))
project3 (Hole _) = Nothing
project3 (Term t) = proj3 t

-- |Project a term to a term over a sub signature.
deepProject :: (Ditraversable f, Difunctor g, g :<: f) => Cxt h f a a
            -> Maybe (Cxt h g a a)
deepProject = appSigFunM proj

-- |Project a term to a term over a binary sub signature.
deepProject2 :: (Ditraversable f, Difunctor g1, Difunctor g2, g1 :<: f, g2 :<: f) => Cxt h f a a -> Maybe (Cxt h (g1 :+: g2) a a)
deepProject2 = appSigFunM proj2

-- |Project a term to a term over a ternary sub signature.
deepProject3 :: (Ditraversable f, Difunctor g1, Difunctor g2, Difunctor g3,
                 g1 :<: f, g2 :<: f, g3 :<: f) => Cxt h f a a
             -> Maybe (Cxt h (g1 :+: g2 :+: g3) a a)
deepProject3 = appSigFunM proj3

-- |A variant of 'deepProject' where the sub signature is required to be
-- 'Traversable rather than the whole signature.
deepProject' :: forall g f h a. (Ditraversable g, g :<: f) => Cxt h f a a
             -> Maybe (Cxt h g a a)
deepProject' val = do
  v <- project val
  v' <- dimapM deepProject' v
  return $ Term v'

-- |A variant of 'deepProject2' where the sub signatures are required to be
-- 'Traversable rather than the whole signature.
deepProject2' :: forall g1 g2 f h a. (Ditraversable g1, Ditraversable g2,
                                      g1 :<: f, g2 :<: f) => Cxt h f a a
             -> Maybe (Cxt h (g1 :+: g2) a a)
deepProject2' val = do
  v <- project2 val
  v' <- dimapM deepProject2' v
  return $ Term v'

-- |A variant of 'deepProject3' where the sub signatures are required to be
-- 'Traversable rather than the whole signature.
deepProject3' :: forall g1 g2 g3 f h a. (Ditraversable g1, Ditraversable g2,
                                         Ditraversable g3, g1 :<: f, g2 :<: f,
                                         g3 :<: f) => Cxt h f a a
             -> Maybe (Cxt h (g1 :+: g2 :+: g3) a a)
deepProject3' val = do
  v <- project3 val
  v' <- dimapM deepProject3' v
  return $ Term v'

{-| A variant of 'inj' for binary sum signatures.  -}
inj2 :: (f1 :<: g, f2 :<: g) => (f1 :+: f2) a e -> g a e
inj2 (Inl x) = inj x
inj2 (Inr y) = inj y

{-| A variant of 'inj' for ternary sum signatures.  -}
inj3 :: (f1 :<: g, f2 :<: g, f3 :<: g) => (f1 :+: f2 :+: f3) a e -> g a e
inj3 (Inl x) = inj x
inj3 (Inr y) = inj2 y

-- |Inject a term where the outermost layer is a sub signature.
inject :: (g :<: f) => g p (Cxt h f p a) -> Cxt h f p a
inject = Term . inj

-- |Inject a term where the outermost layer is a binary sub signature.
inject2 :: (f1 :<: g, f2 :<: g) => (f1 :+: f2) p (Cxt h g p a) -> Cxt h g p a
inject2 = Term . inj2

-- |Inject a term where the outermost layer is a ternary sub signature.
inject3 :: (f1 :<: g, f2 :<: g, f3 :<: g) => (f1 :+: f2 :+: f3) p (Cxt h g p a)
        -> Cxt h g p a
inject3 = Term . inj3

-- |Inject a term over a sub signature to a term over larger signature.
deepInject :: (Difunctor g, Difunctor f, g :<: f) => Cxt h g p a -> Cxt h f p a
deepInject = appSigFun inj

-- |Inject a term over a binary sub signature to a term over larger signature.
deepInject2 :: (Difunctor f1, Difunctor f2, Difunctor g, f1 :<: g, f2 :<: g)
            => Cxt h (f1 :+: f2) p a -> Cxt h g p a
deepInject2 = appSigFun inj2

-- |Inject a term over a ternary signature to a term over larger signature.
deepInject3 :: (Difunctor f1, Difunctor f2, Difunctor f3, Difunctor g,
                f1 :<: g, f2 :<: g, f3 :<: g)
            => Cxt h (f1 :+: f2 :+: f3) p a -> Cxt h g p a
deepInject3 =  appSigFun inj3

injectConst :: (Difunctor g, g :<: f) => Const g -> Cxt h f Nothing a
injectConst = inject . fmap (const undefined)

injectConst2 :: (Difunctor f1, Difunctor f2, Difunctor g, f1 :<: g, f2 :<: g)
             => Const (f1 :+: f2) -> Cxt h g Nothing a
injectConst2 = inject2 . fmap (const undefined)

injectConst3 :: (Difunctor f1, Difunctor f2, Difunctor f3, Difunctor g,
                 f1 :<: g, f2 :<: g, f3 :<: g)
             => Const (f1 :+: f2 :+: f3) -> Cxt h g Nothing a
injectConst3 = inject3 . fmap (const undefined)

projectConst :: (Difunctor g, g :<: f) => Cxt h f Nothing a -> Maybe (Const g)
projectConst = fmap (fmap (const ())) . project

{-| This function injects a whole context into another context. -}
injectCxt :: (Difunctor g, g :<: f) => Cxt h' g p (Cxt h f p a) -> Cxt h f p a
injectCxt (Hole x) = x
injectCxt (Term t) = inject $ fmap injectCxt t

{-| This function lifts the given functor to a context. -}
liftCxt :: (Difunctor f, g :<: f) => g p a -> Context f p a
liftCxt g = simpCxt $ inj g

{-| This function applies the given context with hole type @a@ to a
family @f@ of contexts (possibly terms) indexed by @a@. That is, each
hole @h@ is replaced by the context @f h@. -}

substHoles :: (Difunctor f, Difunctor g, f :<: g) => Cxt h' f p v -> (v -> Cxt h g p a) -> Cxt h g p a
substHoles c f = injectCxt $ fmap f c

substHoles' :: (Difunctor f, Difunctor g, f :<: g, Ord v) => Cxt h' f p v -> Map v (Cxt h g p a) -> Cxt h g p a
substHoles' c m = substHoles c (fromJust . (`Map.lookup`  m))

instance Difunctor f => Monad (Context f a) where
    return = Hole
    (>>=) = substHoles


instance (Show (f a e), Show (g a e)) => Show ((f :+: g) a e) where
    show (Inl v) = show v
    show (Inr v) = show v


instance (Ord (f a e), Ord (g a e)) => Ord ((f :+: g) a e) where
    compare (Inl _) (Inr _) = LT
    compare (Inr _) (Inl _) = GT
    compare (Inl x) (Inl y) = compare x y
    compare (Inr x) (Inr y) = compare x y


instance (Eq (f a e), Eq (g a e)) => Eq ((f :+: g) a e) where
    (Inl x) == (Inl y) = x == y
    (Inr x) == (Inr y) = x == y                   
    _ == _ = False