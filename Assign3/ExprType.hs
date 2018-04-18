module ExprType where

import           Data.List


-- * Section : Datatype declaration
-- | A datatype for common numeric expression
{-
    An expression data type that can encode the following:
-}
data Expr a = Add (Expr a) (Expr a) -- ^ Binary Addition
            | Mult (Expr a) (Expr a) -- ^ Binary Multiplication
            | Cos (Expr a) -- ^ cosine of an expression
            | Sin (Expr a) -- ^ sine of an expression
            | Log (Expr a) -- ^ natural log(base e)
            | NatExp (Expr a) -- ^ natural exponent
            | Exp (Expr a) (Expr a) -- ^ exponent
            | Var String -- ^ variable identifier
            | Const a -- ^ constant wrapper
  deriving Eq

{-getVars :
        Retrieves variable identifiers from an Expr
-}
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Cos e) = getVars e
getVars (Sin e) = getVars e
getVars (Log e) = getVars e
getVars (NatExp e) = getVars e
getVars (Exp e1 e2) = getVars e1 `union` getVars e2
getVars (Var ident)  = [ident]
getVars (Const _)    = []
