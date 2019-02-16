{ 
module TokensMDL where 
}

%wrapper "posn" 
$digit = 0-9     

tokens :-
  $white+       ; 
  "--".*        ; 
  forward   { tok (\p s -> TokenForward p) }
  rotate    { tok (\p s -> TokenRotate p) }
  right     { tok (\p s -> TokenRight p) }
  left      { tok (\p s -> TokenLeft p) }
  \;        { tok (\p s -> TokenSeq p) }
  check     { tok (\p s -> TokenCheck p) }
  [1-9]     { tok (\p s -> TokenDigit p (read s)) }
  if        { tok (\p s -> TokenIf p) }
  then      { tok (\p s -> TokenThen p) }
  else      { tok (\p s -> TokenElse p) }
  \(        { tok (\p s -> TokenLParen p) }
  \)        { tok (\p s -> TokenRParen p) }
  $digit    { tok (\p s -> TokenInt p (read s)) }


{ 

-- Each action has type :: AlexPosn -> String -> Token 

--Helper function
tok f p s = f p s

-- The token type: 
data Token = 
  TokenForward AlexPosn     |
  TokenRotate AlexPosn      |
  TokenRight AlexPosn       |
  TokenLeft AlexPosn        |
  TokenSeq AlexPosn         |
  TokenCheck AlexPosn       |
  TokenDigit AlexPosn Int   |
  TokenIf AlexPosn          |
  TokenThen AlexPosn        |
  TokenElse AlexPosn        |
  TokenLParen AlexPosn      |
  TokenRParen AlexPosn      |
  TokenInt AlexPosn Int     
  deriving (Eq, Show)   

tokenPosn :: Token -> String
tokenPosn (TokenForward (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenRotate (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenRight (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenLeft (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenSeq (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenCheck (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenDigit (AlexPn offset lineNo column) _) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenIf (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenThen (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenElse (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenLParen (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenRParen (AlexPn offset lineNo column)) = show(lineNo) ++ ":" ++ show(column)
tokenPosn (TokenInt (AlexPn offset lineNo column) _) = show(lineNo) ++ ":" ++ show(column)


}
