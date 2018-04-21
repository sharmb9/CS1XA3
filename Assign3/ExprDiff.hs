{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module : ExprDiff
Description : Contains a type class and instances for
differentiable expressions
Copyright : (c) Bilaval Sharma
License : WTFPL
Maintainer : sharmb9@mcmaster.ca
Stability : experimental
Portability : POSIX
-}

module ExprDiff where

import           ExprType

import qualified Data.Map.Strict as Map

{- | Class DiffExpr
   - A type class for Expr datatype which assists with differentiable expressions.
-}
class DiffExpr a where
  -- Given a dictionary and expression of type 'Expr', evaluate an expression
  eval :: Map.Map String a -> Expr a -> a
  -- Given a dictionary of type 'Expr', simplifies the expression as much as it can
  simplify :: Map.Map String a -> Expr a -> Expr a
  -- Given an identifier, performs partial differention w.r.t identifier 
  partDiff :: String -> Expr a -> Expr a
  
  {- Default methods -}
  (!+) :: Expr a -> Expr a -> Expr a
  e1 !+ e2 = simplify (Map.fromList []) $ Add e1 e2

  (!*) :: Expr a -> Expr a -> Expr a
  e1 !* e2 = simplify (Map.fromList []) $ Mult e1 e2

  cosine :: Expr a -> Expr a
  cosine e1 = Cos e1

  sine :: Expr a -> Expr a
  sine e1 = Sin e1

  ln :: Expr a -> Expr a
  ln e1 = Log e1
  
  (!^) :: Expr a -> Expr a -> Expr a
  e1 !^ e2 = simplify (Map.fromList []) $ Exp e1 e2

  expon :: Expr a -> Expr a
  expon e1  = NatExp e1

  neg :: Expr a -> Expr a
  neg e1 =  Const (eval (Map.fromList []) (Neg e1))

  val :: a -> Expr a
  val x = Const x

  var :: String -> Expr a
  var x = Var x


{- |Instances for DiffExpr
-}
instance (Floating a, Eq a) => DiffExpr a where
  -- Evaluates an expression
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Cos e1) = cos (eval vrs e1)
  eval vrs (Sin e1) = sin (eval vrs e1)
  eval vrs (Log e1) = log (eval vrs e1)
  eval vrs (Exp e1 e2) = eval vrs e1 ** eval vrs e2
  eval vrs (NatExp e1) = exp (eval vrs e1)
  eval vrs (Neg e1) = (-1) * (eval vrs e1)
  eval vrs (Const x) = x
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"
  -- Partially or fully differentiates an expression
  partDiff s (Var x) | x == s = (Const 1)
                     | otherwise = (Const 0) 
  partDiff _ (Const _) = Const 0
  partDiff s (Add e1 e2) = Add (partDiff s e1) (partDiff s e2)
  partDiff s (Mult e1 e2) = Add (Mult (partDiff s e1) e2) (Mult e1 (partDiff s e2))
  partDiff s (Log e1) = Mult (Exp e1 (Const (-1))) (partDiff s e1)
  partDiff s (Sin e1) = Mult (Cos e1) (partDiff s e1)
  partDiff s (Cos e1) = Mult (Neg (Sin e1)) (partDiff s e1)
  partDiff s ( NatExp e1) = Mult (NatExp e1) (partDiff s e1)
  
  -- | Simplification of an expression

  -- Simplification of Addition         
  simplify vrs (Add e1 e2) =
    let se1 = simplify vrs e1
        se2 = simplify vrs e2
    in case (se1, se2) of
            (Const 0, se1) -> se1
            (se1, Const 0) -> se1
            (Const a, Const b) -> Const (a+b)
            (Var x, se1) -> Add (Var x) (se1)
            (se1, (Var x)) -> Add (se1) (Var x)
            (se1, se2) -> Add (se1) (se2)

  --Simplification of Multiplication
  simplify vrs (Mult e1 e2) =
    let se1 = simplify vrs e1
        se2 = simplify vrs e2
    in case (se1, se2) of
            (Const a, Const b) -> Const (a*b) 
            (se1, Const 1) -> se1
            (Const 1, se1) -> se1
            (_,Const 0) -> Const 0
            (Const 0,_) -> Const 0
            (Var x, se1) -> Mult (Var x) (se1)
            (se1, Var x) -> Mult (se1) (Var x)
            (se1, se2) -> Mult (se1) (se2)

  --Simplification of Exponent
  simplify vrs (Exp (Const a) (Const b)) = Const (eval vrs (Exp (Const a) (Const b)))
  simplify vrs (Exp e1 e2)=
    let se1 = simplify vrs e1
        se2 = simplify vrs e2
    in case (se1, se2) of
            (Const 0, _) -> Const 0
            (_, Const 0) -> Const 1
            (e1, e2) -> Exp (e1) (e2)

  --Simplification of natural exponent
  simplify vrs (NatExp e1)=
    let se1 = simplify vrs e1
    in case se1 of
             Const 0 -> Const 1
             e1 -> NatExp e1

  --Simplification of Sin
  simplify vrs (Sin e1) = Const (eval vrs (Sin e1))

  --Simplification of Cos
  simplify vrs (Cos e1) = Const (eval vrs (Cos e1))

  --Simplification of Natural Log
  simplify vrs (Log e1) = Const (eval vrs (Log e1))

  --Simplification of negative sign
  simplify vrs (Neg e1) = Mult (Const (-1)) (simplify vrs e1)

  --Simplification of a constant
  simplify vrs (Const a) = Const a

  --Simplification of a variable
  simplify vrs (Var v) = Const (eval vrs (Var v))