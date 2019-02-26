{ 
module LambdaToyGrammar where 
import LambdaToyTokens 
}

%name parseCalc 
%tokentype { Token } 
%error { parseError }
%token 
    int     { TokenInt _ $$}
    true    { TokenTrue _ }
    false   { TokenFalse _ }
    '->'    { TokenFunctionType _ }
    '<'     { TokenLessThan _ }
    '+'     { TokenPlus _ }
    var     { TokenVar _ $$}
    if      { TokenIf _ }
    then    { TokenThen _ }
    else    { TokenElse _ }
    let     { TokenLet _ }
    '='     { TokenEquals _ }
    in      { TokenIn _ }
    '('     { TokenLParen _ }
    ')'     { TokenRParen _ }
    ':'     { TokenColon _ }
    '\\'     { TokenLambda _ }

%right 'in'
%right then
%right else
%right '\\'
%nonassoc '<'
%left '+'
%% 
Type : int              { IntType $1 }
     | true             { BoolTypeTrue }
     | false            { BoolTypeFalse }
     | Type '->' Type   { FunctionType $1 $3 }

Exp : int                                       { Int $1 }
    | true                                      { TrueBoolean }
    | false                                     { FalseBoolean }
    | Exp '<' Exp                               { LessThan $1 $3 }
    | Exp '+' Exp                               { Add $1 $3 }
    | var                                       { Var $1 }
    | if Exp then Exp else Exp                  { Cond $2 $4 $6 }
    | '\\' '(' var ':' Type ')' Exp             { Lambda $3 $5 $7}
    | let '(' var ':' Type ')' '=' Exp in Exp   { Let $3 $5 $8 $10 }
    | Exp Exp                                   { App $1 $2 }
    
{ 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error"
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Type = IntType Int
          | BoolTypeTrue
          | BoolTypeFalse
          | FunctionType Type Type
          deriving Show

data Exp = Int Int
         | TrueBoolean
         | FalseBoolean
         | LessThan Exp Exp
         | Add Exp Exp
         | Var String
         | Cond Exp Exp Exp
         | Lambda String Type Exp
         | Let String Type Exp Exp
         | App Exp Exp
         deriving Show

} 