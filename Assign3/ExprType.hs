{-|
Module      : ExprType
Description : Contains the datatype declaration which defines built-in functions of type `Expr`
Copyright   : (c) Bilaval Sharma @2018
License     : WTFPL
Maintainer  : sharmb9@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}

module ExprType where

import           Data.List


-- * Section : Datatype declaration
-- | A datatype for common numeric expressions
data Expr a = Add (Expr a) (Expr a) -- ^ Binary Addition
            | Mult (Expr a) (Expr a) -- ^ Binary Multiplication
            | Cos (Expr a) -- ^ Cosine of an expression
            | Sin (Expr a) -- ^ Sine of an expression
            | Log (Expr a) -- ^ Natural log(base e)
            | NatExp (Expr a) -- ^ Natural exponent
            | Exp (Expr a) (Expr a) -- ^ Some expression to the power of other expression
            | Neg (Expr a) -- ^ Negative wrapper
            | Var String -- ^ Variable identifier
            | Const a -- ^ Constant wrapper
  deriving Eq

{- | getVars : 
 -gets variable identifier from an Expr
-}
getVars :: Expr a -> [String]
getVars (Add e1 e2)  = getVars e1 `union` getVars e2
getVars (Mult e1 e2) = getVars e1 `union` getVars e2
getVars (Cos e1) = getVars e1
getVars (Sin e1) = getVars e1
getVars (Log e1) = getVars e1
getVars (NatExp e1) = getVars e1
getVars (Exp e1 e2) = getVars e1 `union` getVars e2
getVars (Neg e1) = getVars e1
getVars (Var ident)  = [ident]
getVars (Const _)    = []
