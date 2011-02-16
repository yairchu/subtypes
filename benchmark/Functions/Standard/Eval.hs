module Functions.Standard.Eval where

import DataTypes.Standard
import Functions.Standard.Desugar
import Control.Monad

coerceInt :: (Monad m) => SExpr -> m Int
coerceInt (SInt i) = return i
coerceInt _ = fail ""

coerceBool :: (Monad m) => SExpr -> m Bool
coerceBool (SBool b) = return b
coerceBool _ = fail ""

coercePair :: (Monad m) => SExpr -> m (SExpr,SExpr)
coercePair (SPair x y) = return (x,y)
coercePair _ = fail ""

eval :: (Monad m) => OExpr -> m SExpr
eval (OInt i) = return $ SInt i
eval (OBool b) = return $ SBool b
eval (OPair x y) = liftM2 SPair (eval x) (eval y)
eval (OPlus x y) = liftM2 (\ x y -> SInt (x + y)) (eval x >>= coerceInt) (eval y >>= coerceInt)
eval (OMult x y) = liftM2 (\ x y -> SInt (x * y)) (eval x >>= coerceInt) (eval y >>= coerceInt)
eval (OIf b x y) = eval b >>= coerceBool >>= (\b -> if b then eval x else eval y)
eval (OEq x y) = liftM2 (\ x y -> SBool (x == y)) (eval x) (eval y)
eval (OLt x y) = liftM2 (\ x y -> SBool (x < y)) (eval x >>= coerceInt) (eval y >>= coerceInt)
eval (OAnd x y) = liftM2 (\ x y -> SBool (x && y)) (eval x >>= coerceBool) (eval y >>= coerceBool)
eval (ONot x) = liftM (SBool . not)(eval x >>= coerceBool)
eval (OProj p x) = liftM select (eval x >>= coercePair)
    where select (x,y) = case p of
                           SProjLeft -> x
                           SProjRight -> y

evalSugar :: PExpr -> Err SExpr
evalSugar (PInt i) = return $ SInt i
evalSugar (PBool b) = return $ SBool b
evalSugar (PPair x y) = liftM2 SPair (evalSugar x) (evalSugar y)
evalSugar (PPlus x y) = liftM2 (\ x y -> SInt (x + y)) (evalSugar x >>= coerceInt) (evalSugar y >>= coerceInt)
evalSugar (PMult x y) = liftM2 (\ x y -> SInt (x * y)) (evalSugar x >>= coerceInt) (evalSugar y >>= coerceInt)
evalSugar (PIf b x y) = evalSugar b >>= coerceBool >>= (\b -> if b then evalSugar x else evalSugar y)
evalSugar (PEq x y) = liftM2 (\ x y -> SBool (x == y)) (evalSugar x) (evalSugar y)
evalSugar (PLt x y) = liftM2 (\ x y -> SBool (x < y)) (evalSugar x >>= coerceInt) (evalSugar y >>= coerceInt)
evalSugar (PAnd x y) = liftM2 (\ x y -> SBool (x && y)) (evalSugar x >>= coerceBool) (evalSugar y >>= coerceBool)
evalSugar (PNot x) = liftM (SBool . not)(evalSugar x >>= coerceBool)
evalSugar (PProj p x) = liftM select (evalSugar x >>= coercePair)
    where select (x,y) = case p of
                           SProjLeft -> x
                           SProjRight -> y
evalSugar (PNeg x) = liftM (SInt . negate) (evalSugar x >>= coerceInt)
evalSugar (PMinus x y) = liftM2 (\ x y -> SInt (x - y)) (evalSugar x >>= coerceInt) (evalSugar y >>= coerceInt)
evalSugar (PGt x y) = liftM2 (\ x y -> SBool (x > y)) (evalSugar x >>= coerceInt) (evalSugar y >>= coerceInt)
evalSugar (POr x y) = liftM2 (\ x y -> SBool (x || y)) (evalSugar x >>= coerceBool) (evalSugar y >>= coerceBool)
evalSugar (PImpl x y) = liftM2 (\ x y -> SBool (not x || y)) (evalSugar x >>= coerceBool) (evalSugar y >>= coerceBool)

desugarEval :: PExpr -> Err SExpr
desugarEval = eval . desugar


coerceInt2 :: SExpr -> Int
coerceInt2 (SInt i) = i
coerceInt2 _ = undefined

coerceBool2 :: SExpr -> Bool
coerceBool2 (SBool b) = b
coerceBool2 _ = undefined

coercePair2 :: SExpr -> (SExpr,SExpr)
coercePair2 (SPair x y) = (x,y)
coercePair2 _ = undefined

eval2 :: OExpr -> SExpr
eval2 (OInt i) = SInt i
eval2 (OBool b) = SBool b
eval2 (OPair x y) = SPair (eval2 x) (eval2 y)
eval2 (OPlus x y) = (\ x y -> SInt (x + y)) (coerceInt2 $ eval2 x) (coerceInt2 $ eval2 y)
eval2 (OMult x y) = (\ x y -> SInt (x * y)) (coerceInt2 $ eval2 x) (coerceInt2 $ eval2 y)
eval2 (OIf b x y) = if coerceBool2 $ eval2 b then eval2 x else eval2 y
eval2 (OEq x y) = (\ x y -> SBool (x == y)) (eval2 x) (eval2 y)
eval2 (OLt x y) = (\ x y -> SBool (x < y)) (coerceInt2 $ eval2 x) (coerceInt2 $ eval2 y)
eval2 (OAnd x y) =(\ x y -> SBool (x && y)) (coerceBool2 $ eval2 x) (coerceBool2 $ eval2 y)
eval2 (ONot x) = (SBool . not)(coerceBool2 $ eval2 x)
eval2 (OProj p x) = select (coercePair2 $ eval2 x)
    where select (x,y) = case p of
                           SProjLeft -> x
                           SProjRight -> y


evalSugar2 :: PExpr -> SExpr
evalSugar2 (PInt i) = SInt i
evalSugar2 (PBool b) =  SBool b
evalSugar2 (PPair x y) = SPair (evalSugar2 x) (evalSugar2 y)
evalSugar2 (PPlus x y) = (\ x y -> SInt (x + y)) (coerceInt2 $ evalSugar2 x) (coerceInt2 $ evalSugar2 y)
evalSugar2 (PMult x y) = (\ x y -> SInt (x * y)) (coerceInt2 $ evalSugar2 x) (coerceInt2 $ evalSugar2 y)
evalSugar2 (PIf b x y) = if coerceBool2 $ evalSugar2 b then evalSugar2 x else evalSugar2 y
evalSugar2 (PEq x y) = (\ x y -> SBool (x == y)) (evalSugar2 x) (evalSugar2 y)
evalSugar2 (PLt x y) = (\ x y -> SBool (x < y)) (coerceInt2 $ evalSugar2 x) (coerceInt2 $ evalSugar2 y)
evalSugar2 (PAnd x y) = (\ x y -> SBool (x && y)) (coerceBool2 $ evalSugar2 x) (coerceBool2 $ evalSugar2 y)
evalSugar2 (PNot x) = (SBool . not)(coerceBool2 $ evalSugar2 x)
evalSugar2 (PProj p x) = select (coercePair2 $ evalSugar2 x)
    where select (x,y) = case p of
                           SProjLeft -> x
                           SProjRight -> y
evalSugar2 (PNeg x) = (SInt . negate) (coerceInt2 $ evalSugar2 x)
evalSugar2 (PMinus x y) = (\ x y -> SInt (x - y)) (coerceInt2 $ evalSugar2 x) (coerceInt2 $ evalSugar2 y)
evalSugar2 (PGt x y) = (\ x y -> SBool (x > y)) (coerceInt2 $ evalSugar2 x) (coerceInt2 $ evalSugar2 y)
evalSugar2 (POr x y) = (\ x y -> SBool (x || y)) (coerceBool2 $ evalSugar2 x) (coerceBool2 $ evalSugar2 y)
evalSugar2 (PImpl x y) = (\ x y -> SBool (not x || y)) (coerceBool2 $ evalSugar2 x) (coerceBool2 $ evalSugar2 y)

desugarEval2 :: PExpr -> SExpr
desugarEval2 = eval2 . desugar




coerceHOASInt2 :: HOASSExpr -> Int
coerceHOASInt2 (HOASSInt i) = i
coerceHOASInt2 _ = undefined

coerceHOASBool2 :: HOASSExpr -> Bool
coerceHOASBool2 (HOASSBool b) = b
coerceHOASBool2 _ = undefined

coerceHOASPair2 :: HOASSExpr -> (HOASSExpr,HOASSExpr)
coerceHOASPair2 (HOASSPair x y) = (x,y)
coerceHOASPair2 _ = undefined

coerceHOASLam2 :: HOASSExpr -> HOASSExpr -> HOASSExpr
coerceHOASLam2 (HOASSLam f) = f
coerceHOASLam2 _ = undefined

evalHOAS :: HOASExpr -> HOASSExpr
evalHOAS (HOASInt i) = HOASSInt i
evalHOAS (HOASBool b) = HOASSBool b
evalHOAS (HOASPair x y) = HOASSPair (evalHOAS x) (evalHOAS y)
evalHOAS (HOASPlus x y) = (\ x y -> HOASSInt (x + y)) (coerceHOASInt2 $ evalHOAS x) (coerceHOASInt2 $ evalHOAS y)
evalHOAS (HOASMult x y) = (\ x y -> HOASSInt (x * y)) (coerceHOASInt2 $ evalHOAS x) (coerceHOASInt2 $ evalHOAS y)
evalHOAS (HOASIf b x y) = if coerceHOASBool2 $ evalHOAS b then evalHOAS x else evalHOAS y
evalHOAS (HOASEq x y) = (\ x y -> HOASSBool (x == y)) (evalHOAS x) (evalHOAS y)
evalHOAS (HOASLt x y) = (\ x y -> HOASSBool (x < y)) (coerceHOASInt2 $ evalHOAS x) (coerceHOASInt2 $ evalHOAS y)
evalHOAS (HOASAnd x y) =(\ x y -> HOASSBool (x && y)) (coerceHOASBool2 $ evalHOAS x) (coerceHOASBool2 $ evalHOAS y)
evalHOAS (HOASNot x) = (HOASSBool . not)(coerceHOASBool2 $ evalHOAS x)
evalHOAS (HOASProj p x) = select (coerceHOASPair2 $ evalHOAS x)
    where select (x,y) = case p of
                           SProjLeft -> x
                           SProjRight -> y
evalHOAS (HOASApp x y) = (coerceHOASLam2 $ evalHOAS x) (evalHOAS y)
evalHOAS (HOASLam f) = HOASSLam $ evalHOAS . f
evalHOAS (HOASVal v) = v