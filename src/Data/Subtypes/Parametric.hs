{-# LANGUAGE GHC2021, DataKinds, TypeFamilies, UndecidableInstances #-}

-- | Subsume class for parametric types.
--
-- Based on parts of Patrick Bahr's compdata:Data.Comp.Ops module.

module Data.Subtypes.Parametric where

import Data.Proxy (Proxy(..))
import GHC.Generics

import Data.Subtypes.Internal


infixl 5 :<:

type family Elem f g where
    Elem f f = Found Here
    Elem f (M1 _ _ g) = Seq' Wrapped (Elem f g)
    Elem (M1 _ _ f) g = Seq' Unwrapped (Elem f g)
    Elem (f1 :+: f2) g = Sum' (Elem f1 g) (Elem f2 g)
    Elem f (g1 :+: g2) = Choose (Elem f g1) (Elem f g2)
    Elem f g = NotFound

class Subsume (e :: Emb Pos) f g where
    inj' :: Proxy e -> f a -> g a
    prj' :: Proxy e -> g a -> Maybe (f a)

instance Subsume (Found Here) f f where
    inj' _ = id

    prj' _ = Just

instance Subsume (Found p) f g => Subsume (Found (Le p)) f (g :+: g') where
    inj' _ = L1 . inj' (Proxy @(Found p))

    prj' _ (L1 x) = prj' (Proxy @(Found p)) x
    prj' _ _ = Nothing

instance Subsume (Found p) f g => Subsume (Found (Ri p)) f (g' :+: g) where
    inj' _ = R1 . inj' (Proxy @(Found p))

    prj' _ (R1 x) = prj' (Proxy @(Found p)) x
    prj' _ _ = Nothing

instance (Subsume (Found p1) f1 g, Subsume (Found p2) f2 g) =>
    Subsume (Found (Sum p1 p2)) (f1 :+: f2) g where
    inj' _ (L1 x) = inj' (Proxy @(Found p1)) x
    inj' _ (R1 x) = inj' (Proxy @(Found p2)) x

    prj' _ x =
        case prj' (Proxy @(Found p1)) x of
        Just y -> Just (L1 y)
        _ ->
            case prj' (Proxy @(Found p2)) x of
            Just y -> Just (R1 y)
            _ -> Nothing

instance (Subsume (Found p) f g) => Subsume (Found (Wrapped p)) f (M1 i c g) where
    inj' _ = M1 . inj' (Proxy @(Found p))

    prj' _ (M1 x) = prj' (Proxy @(Found p)) x

instance (Subsume (Found p) f g) => Subsume (Found (Unwrapped p)) (M1 i c f) g where
    inj' _ (M1 x) = inj' (Proxy @(Found p)) x

    prj' _ = fmap M1 . prj' (Proxy @(Found p))

-- | A constraint @f :<: g@ expresses that the signature @f@ is
-- subsumed by @g@, i.e. @f@ can be used to construct elements in @g@.
type f :<: g = Subsume (ComprEmb (Elem f g)) f g

inj :: forall f g a. f :<: g => f a -> g a
inj = inj' (Proxy @(ComprEmb (Elem f g)))

proj :: forall f g a. f :<: g => g a -> Maybe (f a)
proj = prj' (Proxy @(ComprEmb (Elem f g)))
