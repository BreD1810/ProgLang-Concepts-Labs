{ 
module MDLTokens where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 

tokens :-
$white+       ; 
  "--".*        ; 
  Forward        { tok (\p s -> TokenForward p)} 
  Rotate         { tok (\p s -> TokenRotate p) }
  [1-9]          { tok (\p s -> TokenDigit p (read s)) } 
  $digit $digit+ { tok (\p s -> TokenInt p (read s)) }
  Check          { tok (\p s -> TokenCheck p) }
  If             { tok (\p s -> TokenIf p) }
  Then           { tok (\p s -> TokenThen p) }
  Else           { tok (\p s -> TokenElse p) }
  L              { tok (\p s -> TokenLeft p) }
  R              { tok (\p s -> TokenRight p) }
  \;             { tok (\p s -> TokenSeq p )}
  \(             { tok (\p s -> TokenLParen p) }
  \)             { tok (\p s -> TokenRParen p) }

{ 
-- Each action has type :: AlexPosn -> String -> MDLToken 

-- Helper function
tok f p s = f p s

-- The token type: 
data MDLToken = 
  TokenForward AlexPosn        | 
  TokenRotate  AlexPosn        | 
  TokenDigit AlexPosn Int      |
  TokenInt AlexPosn Int        | 
  TokenCheck AlexPosn          |
  TokenIf AlexPosn             |
  TokenThen AlexPosn           |
  TokenElse AlexPosn           |
  TokenLeft AlexPosn           |
  TokenRight AlexPosn          |
  TokenSeq AlexPosn            |
  TokenLParen AlexPosn         |
  TokenRParen AlexPosn      
  deriving (Eq,Show) 

tokenPosn :: MDLToken -> String
tokenPosn (TokenForward (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRotate  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenDigit  (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenInt  (AlexPn a l c) _) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenCheck  (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenIf (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenThen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenElse (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLeft (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRight (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenSeq (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenLParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)
tokenPosn (TokenRParen (AlexPn a l c)) = show(l) ++ ":" ++ show(c)


}