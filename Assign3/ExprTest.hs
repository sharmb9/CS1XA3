{-|
Module : ExprTest
Description : Contains test cases for the library
Copyright : (c) Bilaval Sharma
License : WTFPL
Maintainer : sharmb9@mcmaster.ca
Stability : experimental
Portability : POSIX
-}

module ExprTest(evalCase1, evalCase2, evalCase3, simplifyCase1, simplifyCase2, diffCase) where

import           ExprDiff
import           ExprParser
import           ExprType

import qualified Data.Map.Strict as Map
import           Test.QuickCheck

-- |Test case for addition
evalCase1 :: Double -> Double -> Bool
evalCase1 a b = eval (Map.fromList [("x",a),("y",b)]) (Add (Var "x") (Var "y")) == a+b
testevalCase1 = quickCheck evalCase1

-- |Test case for multiplication
evalCase2 :: Double -> Double -> Bool
evalCase2 a b = eval (Map.fromList [("x",a),("y",b)]) (Mult (Var "x") (Var "y")) == a*b
testevalCase2 = quickCheck evalCase2

-- |Test case for Exponent
evalCase3 :: Double -> Double -> Bool
evalCase3 a b = eval (Map.fromList [("x",a),("y",b)]) (Exp (Var "x") (Var "y")) == a**b
testevalCase3 = quickCheck evalCase2

-- |Test case to check simplification of multiplication of a variable with constant
simplifyCase1 :: Double -> Double -> Bool
simplifyCase1 a b  = simplify (Map.fromList [("x",a),("y",b)]) (Mult (Const 1) (Var "x")) == Const a
testsimplifyCase1 = quickCheck simplifyCase1

-- |Test case to check exponent
simplifyCase2 :: Double -> Double -> Bool
simplifyCase2 a b= simplify (Map.fromList [("x",a),("y",b)]) (Exp (Const 4) (Const 0)) == Const 1

-- |Test case to check partial differentiation of a constant
diffCase :: String -> Double -> Bool
diffCase s a = partDiff s (Const a) == Const 0
testdiffCase = quickCheck diffCase
