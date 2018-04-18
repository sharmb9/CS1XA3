{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}
module ExprDiff where

import           ExprType

import qualified Data.Map.Strict as Map

{-|
Module : ExprDiff
Description : Contains a type class and instances for
differentiable expressions
Copyright : (c) Bilaval Sharma
License : WTFPL
Maintainer : sharmb9@mcmaster.ca
Stability : experimental
Portability : POSIX
TODO write a longer description of the module,
containing some commentary with @some markup@.
-}

class DiffExpr a where
  eval :: Map.Map String a -> Expr a -> a
  simplify :: Map.Map String a -> Expr a -> Expr a
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

  val :: a -> Expr a
  val x = Const x

  var :: String -> Expr a
  var x = Var x



--cite barskyn for this
instance (Floating a, Eq a) => DiffExpr a where
  eval vrs (Add e1 e2)  = eval vrs e1 + eval vrs e2
  eval vrs (Mult e1 e2) = eval vrs e1 * eval vrs e2
  eval vrs (Cos e1) = cos (eval vrs e1)
  eval vrs (Sin e1) = sin (eval vrs e1)
  eval vrs (Log e1) = log (eval vrs e1)
  eval vrs (Exp e1 e2) = eval vrs e1 ** eval vrs e2
  eval vrs (NatExp e1) = exp (eval vrs e1)
  eval vrs (Const x) = x
  eval vrs (Var x) = case Map.lookup x vrs of
                       Just v  -> v
                       Nothing -> error "failed lookup in eval"
  partDiff s (Var x) | x == s = (Const 1)
                     | otherwise = (Const 0) 
  partDiff _ (Const _) = Const 0
  partDiff s (Add e1 e2) = Add (partDiff s e1) (partDiff s e2)
  partDiff s (Mult e1 e2) = Add (Mult (partDiff s e1) e2) (Mult e1 (partDiff s e2))
  partDiff s (Log e1) = Mult (Exp e1 (Const (-1))) (partDiff s e1)
  partDiff s (Sin e1) = Mult (Cos e1) (partDiff s e1)
  partDiff s (Cos e1) = Mult (Mult (Const (-1)) (Sin e1)) (partDiff s e1)
  partDiff s ( NatExp e1) = Mult (NatExp e1) (partDiff s e1)
            {- Simplify -} --TODO: Cite https://www.kovach.me/posts/2013-05-01-symbolic-calculus.html for reference
  simplify vrs (Add (Const a) (Const b)) = Const (a+b)
  simplify vrs (Add e1 (Const 0)) = simplify vrs e1
  simplify vrs (Add (Const 0) e1) = simplify vrs e1
  simplify vrs (Add (Var x) e1) = Add (Var x) (simplify vrs e1)
  simplify vrs (Add e1 (Var x)) = Add (simplify vrs e1) (Var x)
  simplify vrs (Add e1 e2) = Add (simplify vrs e1) (simplify vrs e2)

      {--multiplication--}
  simplify vrs (Mult (Const a) (Const b)) = Const (a*b)
  simplify vrs (Mult e1 (Const 1)) = simplify vrs e1
  simplify vrs (Mult (Const 1) e1) = simplify vrs e1
  simplify vrs (Mult e1 (Const 0)) = Const 0
  simplify vrs (Mult (Const 0) e1) = Const 0
  simplify vrs (Mult (Var x) e1) = Mult (Var x) (simplify vrs e1)
  simplify vrs (Mult e1 (Var x)) = Mult (simplify vrs e1) (Var x)
  simplify vrs (Mult e1 e2) = Mult (simplify vrs e1) (simplify vrs e2)

  simplify vrs (Exp e1 e2) = Const (eval vrs (Exp e1 e2))  
  simplify vrs (Sin e1) = Const (eval vrs (Sin e1))
  simplify vrs (Cos e1) = Const (eval vrs (Cos e1))
  simplify vrs (Log e1) = Const (eval vrs (Log e1))
  simplify vrs (Const a) = Const a
  simplify vrs (Var v) = Const (eval vrs (Var v))