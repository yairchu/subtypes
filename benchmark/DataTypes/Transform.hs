{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses,
  FlexibleInstances,
  FlexibleContexts,
  UndecidableInstances,
  TypeOperators,
  ScopedTypeVariables,
  TypeSynonymInstances #-}

module DataTypes.Transform where

import Data.Comp
import Data.Comp.ExpFunctor
import DataTypes.Standard as S
import DataTypes.Comp

class TransSugar f where
    transSugarAlg :: Alg f PExpr

transSugar :: (Functor f, TransSugar f) => Term f -> PExpr
transSugar = cata transSugarAlg

instance (TransSugar f, TransSugar g) => TransSugar (f :+: g) where
    transSugarAlg (Inl v) = transSugarAlg v
    transSugarAlg (Inr v) = transSugarAlg v

instance TransSugar Value where
    transSugarAlg (VInt i) = PInt i
    transSugarAlg (VBool b) = PBool b
    transSugarAlg (VPair x y) = PPair x y

instance TransSugar Op where
    transSugarAlg (Plus x y) = PPlus x y
    transSugarAlg (Mult x y) = PMult x y
    transSugarAlg (If b x y) = PIf b x y
    transSugarAlg (Lt x y) = PLt x y
    transSugarAlg (And x y) = PAnd x y
    transSugarAlg (Not x) = PNot x
    transSugarAlg (Proj p x) = PProj (ptrans p) x
        where ptrans ProjLeft = SProjLeft
              ptrans ProjRight = SProjRight
    transSugarAlg (Eq x y) = PEq x y

instance TransSugar Sugar where
    transSugarAlg (Neg x) = PNeg x
    transSugarAlg (Minus x y) = PMinus x y
    transSugarAlg (Gt x y) = PGt x y
    transSugarAlg (Or x y) = POr x y
    transSugarAlg (Impl x y) = PImpl x y

class TransHOAS f where
    transHOASAlg :: Alg f S.HOASExpr

transHOAS :: (ExpFunctor f, TransHOAS f) => Term f -> S.HOASExpr
transHOAS = cataE transHOASAlg

instance (TransHOAS f, TransHOAS g) => TransHOAS (f :+: g) where
    transHOASAlg (Inl v) = transHOASAlg v
    transHOASAlg (Inr v) = transHOASAlg v

instance TransHOAS Value where
    transHOASAlg (VInt i) = HOASInt i
    transHOASAlg (VBool b) = HOASBool b
    transHOASAlg (VPair x y) = HOASPair x y

instance TransHOAS Op where
    transHOASAlg (Plus x y) = HOASPlus x y
    transHOASAlg (Mult x y) = HOASMult x y
    transHOASAlg (If b x y) = HOASIf b x y
    transHOASAlg (Lt x y) = HOASLt x y
    transHOASAlg (And x y) = HOASAnd x y
    transHOASAlg (Not x) = HOASNot x
    transHOASAlg (Proj p x) = HOASProj (ptrans p) x
        where ptrans ProjLeft = SProjLeft
              ptrans ProjRight = SProjRight
    transHOASAlg (Eq x y) = HOASEq x y

instance TransHOAS Lam where
    transHOASAlg (Lam f) = HOASLam $ f . HOASVal

instance TransHOAS App where
    transHOASAlg (App x y) = HOASApp x y