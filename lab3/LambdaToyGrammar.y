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
    Int     { TokenIntType _ }
    Bool    { TokenBoolType _ }
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

%right then
%right else
%right in
%right '\\'
%nonassoc '<'
%left '+'
%% 

Exp : int                                           { Int $1 }
    | true                                          { TrueBoolean }
    | false                                         { FalseBoolean }
    | Exp '<' Exp                                   { LessThan $1 $3 }
    | Exp '+' Exp                                   { Add $1 $3 }
    | var                                           { Var $1 }
    | if Exp then Exp else Exp                      { Cond $2 $4 $6 }
    | '\\' '(' var ':' Type ')' Exp                 { Lambda $3 $5 $7}
    | let var '=' Exp in Exp                        { Let $2 $4 $6 }
    | Exp Exp                                       { App $1 $2 }
    | '(' Exp ')'                                   { $2 }

Type : Int              { IntType }
     | Bool             { BoolType }
     | Type '->' Type   { FunctionType $1 $3 }

{ 
parseError :: [Token] -> a
parseError [] = error "Unknown Parse Error"
parseError (t:ts) = error ("Parse error at line:column " ++ (tokenPosn t))

data Exp = Int Int
         | TrueBoolean
         | FalseBoolean
         | LessThan Exp Exp
         | Add Exp Exp
         | Var String
         | Cond Exp Exp Exp
         | Lambda String Type Exp
         | Let String Exp Exp
         | App Exp Exp
         deriving Show

data Type = IntType
          | BoolType
          | FunctionType Type Type
          deriving Show

} 