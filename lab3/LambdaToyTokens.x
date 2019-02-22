{ 
module LambdaToyTokens where 
}

%wrapper "posn" 
$digit = 0-9
$char = a-z

tokens :-
  $white+       ; 
  "--".*        ; 
  $digit    { tok (\p s -> TokenInt p (read s)) }
  true      { tok (\p s -> TokenTrue p) }
  false     { tok (\p s -> TokenFalse p)}
  '->'        { tok (\p s -> TokenFunctionType p) }
  \<        { tok (\p s -> TokenLessThan p) }
  \+        { tok (\p s -> TokenPlus p) }
  $char     { tok (\p s -> TokenVar p (read s))}
  if        { tok (\p s -> TokenIf p) }
  then      { tok (\p s -> TokenThen p) }
  else      { tok (\p s -> TokenElse p) }
  let       { tok (\p s -> TokenLet p) }
  \=        { tok (\p s -> TokenEquals p) }
  in        { tok (\p s -> TokenIn p) }
  \(        { tok (\p s -> TokenLParen p) }
  \)        { tok (\p s -> TokenRParen p) }
  \:        { tok (\p s -> TokenColon p) }
  \\        { tok (\p s -> TokenLambda p) }

{ 

-- Each action has type :: AlexPosn -> String -> Token 

--Helper function
tok f p s = f p s

-- The token type: 
data Token = 
  TokenInt AlexPosn Int       |
  TokenTrue AlexPosn          |
  TokenFalse AlexPosn         |
  TokenFunctionType AlexPosn  |
  TokenLessThan AlexPosn      |
  TokenPlus AlexPosn          |
  TokenVar AlexPn char        |
  TokenIf AlexPosn            |
  TokenThen AlexPosn          |
  TokenElse AlexPosn          |
  TokenLet AlexPosn           |
  TokenEquals AlexPosn        |
  TokenIn AlexPosn            |
  TokenLParen AlexPosn        |
  TokenRParen AlexPosn        |
  TokenColon AlexPosn         |
  TokenLambda AlexPosn
  deriving (Eq, Show)   

tokenPosn :: Token -> String
tokenPosn (TokenInt (AlexPn offset lineNo column) _) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenTrue (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenFalse (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenFunctionType (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenLessThan (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenPlus (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenVarr (AlexPn offset lineNo column) _) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenIf (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenThen (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenElse (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenLet (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenEquals (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenIn (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenLParen (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenEquals (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenColon (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenLambda (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)

}
