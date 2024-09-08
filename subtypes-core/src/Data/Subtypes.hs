{-# LANGUAGE GHC2021, DataKinds, TypeFamilies, UndecidableInstances #-}

module Data.Subtypes (Subsume(..), (:<), inj, proj) where

import Data.Proxy (Proxy(..))
import GHC.Generics

import Data.Subtypes.Internal
import qualified Data.Subtypes.Parametric as P

type IsSubtype a b =
    CheckSubtype
    (P.Elem (Rec0 a) (Rep b))
    (CheckStructureSubtype (IsBuiltin a) a b)

type family CheckSubtype nom str where
    CheckSubtype (Found p) NotFound = Found (ByName p)
    CheckSubtype NotFound (Found p) = Found (ByStructure p)
    CheckSubtype NotFound NotFound = NotFound
    CheckSubtype _ _ = Ambiguous

type family CheckStructureSubtype i a b where
    CheckStructureSubtype True _ _ = NotFound
    CheckStructureSubtype False a b = P.Elem (Rep a) (Rep b)

type family IsBuiltin a where
    IsBuiltin Int = True
    IsBuiltin Float = True
    IsBuiltin Double = True
    IsBuiltin Char = True
    IsBuiltin _ = False

class Subsume (e :: Emb SubtypePos) a b where
    inj' :: Proxy e -> a -> b
    prj' :: Proxy e -> b -> Maybe a

instance (Generic b, P.Subsume (Found p) (Rec0 a) (Rep b)) =>
    Subsume (Found (ByName p)) a b where
    inj' _ = to . P.inj' (Proxy @(Found p)) . K1 @_ @R
    prj' _ = fmap (unK1 @_ @R) . P.prj' (Proxy @(Found p)) . from

instance (Generic a, Generic b, P.Subsume (Found p) (Rep a) (Rep b)) =>
    Subsume (Found (ByStructure p)) a b where
    inj' _ = to . P.inj' (Proxy @(Found p)) . from
    prj' _ = fmap to . P.prj' (Proxy @(Found p)) . from

infixl 5 :<
type a :< b = Subsume (IsSubtype a b) a b

inj :: forall a b. a :< b => a -> b
inj = inj' (Proxy @(IsSubtype a b))

proj :: forall a b. a :< b => b -> Maybe a
proj = prj' (Proxy @(IsSubtype a b))
