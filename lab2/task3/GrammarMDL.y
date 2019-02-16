{ 
module GrammarMDL where 
import TokensMDL 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
    forward { TokenForward _ }
    rotate  { TokenRotate _ }
    right       { TokenRight _ }
    left       { TokenLeft _ }
    ';'     { TokenSeq _ }
    check   { TokenCheck _ }
    digit   { TokenDigit _ $$ }
    if      { TokenIf _ }
    then    { TokenThen _ }
    else    { TokenElse _ }
    '('     { TokenLParen _ }
    ')'     { TokenRParen _ }
    int     { TokenInt _ $$}

%right then
%right else
%right ';'
%left 'forward'
%left 'rotate'
%% 
Exp : forward int               { Forward $2 }
    | forward digit             { Forward $2 }
    | rotate Direction          { Rotate $2 }
    | check digit               { Check $2 }
    | if Exp then Exp else Exp  { Cond $2 $4 $6 }
    | Exp ';' Exp               { Seq $1 $3 }
    | '(' Exp ')'               { $2 }

Direction : l { LeftDirection }
          | r { RightDirection }

    
{ 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error"
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Exp = Forward Int 
         | Rotate Direction
         | Check Int
         | Cond Exp Exp Exp
         | Seq Exp Exp
         deriving Show
data Direction = LeftDirection | RightDirection deriving Show
} 