{ 
module Tokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
  $white+       ; 
  "--".*        ; 
  let           { tok ( \p s -> TokenLet p)} 
  in            { tok (\p s -> TokenIn p)}
  $digit+       { tok (\p s -> TokenInt p (read s))} 
  \=          { tok (\p s -> TokenEq p)}
  \+          { tok (\p s -> TokenPlus p)}
  \-          { tok (\p s -> TokenMinus p)}
  \*          { tok (\p s -> TokenTimes p)}
  \/          { tok (\p s -> TokenDiv p)}
  \(          { tok (\p s -> TokenLParen p)}
  \)          { tok (\p s -> TokenRParen p)}
  \^          { tok (\p s -> TokenExp p)}
  $alpha [$alpha $digit \_ \’]*   { tok (\p s -> TokenVar p s)} 

{ 

-- Each action has type :: AlexPosn -> String -> Token 

--Helper function
tok f p s = f p s

-- The token type: 
data Token = 
  TokenLet AlexPosn         | 
  TokenIn AlexPosn          | 
  TokenInt AlexPosn Int     |
  TokenVar AlexPosn String  | 
  TokenEq AlexPosn          |
  TokenPlus AlexPosn        |
  TokenMinus AlexPosn       |
  TokenTimes AlexPosn       |
  TokenDiv AlexPosn         |
  TokenLParen AlexPosn      |
  TokenRParen AlexPosn      |
  TokenExp AlexPosn
  deriving (Eq,Show) 

tokenPosn :: Token -> String
tokenPosn (TokenLet (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenIn (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenInt (AlexPn offset lineNo column) n) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenVar (AlexPn offset lineNo column) x) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenEq (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenPlus (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenMinus (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenTimes (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenDiv (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenLParen (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenRParen (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenExp (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)

}
