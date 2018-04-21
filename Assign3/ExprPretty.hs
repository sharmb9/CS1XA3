{-|
Module      : ExprPretty
Description : Contains show instances for 'Expr'
Copyright   : (c) Bilaval Sharma @2018
License     : WTFPL
Maintainer  : sharmb9@mcmaster.ca
Stability   : experimental
Portability : POSIX
-}

module ExprPretty where

import           ExprType

parens :: String -> String
parens ss = "(" ++ ss ++ ")"

--Prints the operator associated with each operation
instance Show a => Show (Expr a) where
  show (Add e1 e2)  = parens (show e1) ++ " !+ " ++ parens (show e2)
  show (Mult e1 e2) = parens (show e1) ++ " !* " ++ parens (show e2)
  show (Cos e1) = parens $ "cosine"++(show e1)
  show (Sin e1) = parens $ "sine"++(show e1)
  show (Log e1) = parens $ "ln"++(show e1)
  show (Exp e1 e2) = parens (show e1) ++ "!^" ++ parens (show e2)
  show (NatExp e1) = parens $ "expon"++(show e1)
  show (Var ss)     = parens $ "var \"" ++ ss ++ "\""
  show (Const x)    = parens $ "val " ++ show x