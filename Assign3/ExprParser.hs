{-|
    Module : ExprParser
    Description : Contains parsers that take a string and 
                  output an 'Expr' datatype expression 
    Copyright : (c) Bilaval Sharma @2018
    License : WTFPL
    Maintainer : sharmb9@mcmaster.ca
    Stability : experimental
    Portability : POSIX
    Depend : This module depends on the "ExprType" module
-}



module ExprParser (parseExprD, parseExprDnlsc, parseExprF, parseExprFnlsc) where

import           ExprType

import           Text.Parsec
import           Text.Parsec.String

{- | This parser takes binary construcotrs( +, *, **) and parses strings numbers into the Double type of an Expr constructor -}
parseExprD :: String -> Expr Double 
parseExprD ss = case parse setExprD "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

setExprD :: Parser (Expr Double)
setExprD = termD `chainl1` setOp

termD :: Parser (Expr Double)
termD = (notOp factorD) <|> factorD

factorD :: Parser (Expr Double)
factorD = try doubleParse <|> variable

doubleParse :: Parser (Expr Double)
doubleParse = do { c <- double;
                   return $ Const c}

{- | This parser takes unary constructors(natrual log, natural exponent, sin, cos) and parses strings numbers into the Double type of an Expr constructor  -}
parseExprDnlsc :: String -> Expr Double
parseExprDnlsc ss = case parse setExprDnlsc "" ss of
                     Left err -> error $ show err
                     Right expr -> expr

setExprDnlsc :: Parser (Expr Double)
setExprDnlsc = let 
              opNlsc = do { op <- setOpNlsc;
                           spaces;
                           term <- termDnlsc;
                           spaces;
                           return $ op term }
                in try opNlsc <|> termDnlsc

termDnlsc :: Parser (Expr Double)
termDnlsc = (notOp factorD) <|> factorD


{- | This parser takes binary construcotrs( +, *, **) and parses strings numbers into the Float type of an Expr constructor -}
parseExprF :: String -> Expr Float
parseExprF ss = case parse setExprF "" ss of
                  Left err   -> error $ show err
                  Right expr -> expr

setExprF :: Parser (Expr Float)
setExprF = termF `chainl1` setOp

termF :: Parser (Expr Float)
termF = (notOp factorF) <|> factorF

factorF :: Parser (Expr Float)
factorF = try floatParse <|> variable

{- | This parser takes unary constructors(natrual log, natural exponent, sin, cos) and parses strings numbers into the Float type of an Expr constructor  -}
parseExprFnlsc :: String -> Expr Float
parseExprFnlsc ss = case parse setExprFnlsc "" ss of 
                    Left err   -> error $ show err
                    Right expr -> expr

setExprFnlsc :: Parser (Expr Float)
setExprFnlsc = let opNlsc = do { op <- setOpNlsc; 
                               spaces;
                               term <- termFnlsc;
                               spaces;
                               return $ op term}
                in try opNlsc <|> termFnlsc

termFnlsc :: Parser (Expr Float)
termFnlsc = (notOp factorF) <|> factorF

floatParse :: Parser (Expr Float)
floatParse = do { c <- float;
                  return $ Const c }

{-variable parser-}
variable :: Parser (Expr a)
variable = do {var <- many1 letter;
               return $ Var var }

{-binary operator parser-}
setOp :: Parser (Expr a -> Expr a -> Expr a)
setOp = do {symbol "+"; return Add }
    <|> do {symbol "*"; return Mult }
    <|> do {symbol "^"; return Exp }

{-unary operator parser-}
setOpNlsc :: Parser (Expr a -> Expr a)
setOpNlsc = do { string "cos"; return Cos }
       <|> do { string "sin"; return Sin }
       <|> do { string "log"; return Log }
       <|> do { string "exp"; return NatExp }

{-parses the unary operator "-" -}
notOp :: Parser (Expr a) -> Parser (Expr a)
notOp p = do { symbol "-";
               expr <- p;
               return $ Neg expr }



{- Utility Combinators -}

symbol :: String -> Parser String
symbol ss = let
  symbol' :: Parser String
  symbol' = do { spaces;
                 ss' <- string ss;
                 spaces;
                 return ss' }
  in try symbol'

digits :: Parser String
digits = many1 digit

negDigits :: Parser String
negDigits = do { neg <- symbol "-" ;
                    dig <- digits ;
                    return (neg ++ dig) }

integer :: Parser Integer
integer = fmap read $ try negDigits <|> digits

int :: Parser Int
int = fmap read $ try negDigits <|> digits

doubleDigits :: Parser String
doubleDigits = do { ds <- try negDigits <|> digits ;
                    rs <- try decimalDigits <|> return "" ;
                    return $ ds ++ rs }

decimalDigits :: Parser String
decimalDigits = do { d <- char '.' ;
                     rm <- digits ;
                     return $ d:rm }

double :: Parser Double
double = fmap read $ doubleDigits

float:: Parser Float
float = fmap read $ doubleDigits
