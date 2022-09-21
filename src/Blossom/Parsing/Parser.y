{
module Blossom.Parsing.Parser (
    blossomParser,
) where

import Blossom.Parsing.AbsSynTree
import Blossom.Parsing.Lexer
import Blossom.Parsing.Token
import Blossom.Typing.Type
}


%name blossomParser Module
%error { parseError }
%lexer { lexer } { TokEnd }
%monad { Alex }

%tokentype { Token }
%token
    integer                 { TokInteger $$ }
    float                   { TokFloat $$ }
    string                  { TokString $$ }
    operator                { TokOperator $$ }
    small_id                { TokSmallId $$ }
    big_id                  { TokBigId $$ }
    ";"                     { TokSemi }
    ":"                     { TokColon }
    "::"                    { TokDoubleColon }
    "->"                    { TokArrow }
    "=>"                    { TokEqArrow }
    "("                     { TokLParen }
    ")"                     { TokRParen }
    "{"                     { TokLBrace }
    "}"                     { TokRBrace }
    import                  { TokImport }
    func                    { TokFunc }
    data                    { TokData }
    match                   { TokMatch }

-- ghost precedence for function application
-- https://stackoverflow.com/questions/27630269
%nonassoc APP

%%

Module :: { ModuleAST }
    : ImportList TopLevel { ModuleAST $1 $2 }

ImportList :: { [Import] }
    : ImportList Import { ($2:$1) }
    | {- EMPTY -} { [] }

Import :: { Import }
    : import big_id ";" { Import $2 }

TopLevel :: { [TopLevelExpr] }
    : TopLevel TopLevelExpr { ($2:$1) }
    | {- EMPTY -} { [] }

TopLevelExpr :: { TopLevelExpr }
    : FunctionDefinition { FuncDef $1 }
    | DataDefinition { DataDef $1 }

FunctionDefinition :: { Function }
    : func small_id "::" Signature StmtAssignment
        { Function $2 (fst $4) (snd $4) $5 }

Signature :: { ([Param], Type) }
    : Params "->" Type { ($1, $3) }
    | Type { ([], $1) }

Params :: { [Param] }
    : Params_ { reverse $1}

Params_ :: { [Param] }
    : Params_ "->" Param { ($3:$1) }
    | Param { [$1] }

Param :: { Param }
    : small_id ":" Type { Param $1 $3 }

StmtAssignment :: { Expr }
    : Assignment ";" { $1 }

Assignment :: { Expr }
    : "=>" Expr { $2 }

Expr :: { Expr }
    : Term { $1 }
    | Term operator Term { FuncApp (FuncApp (VarExpr $2) $1) $3 }
    -- | Signature Assignment { Lambda (fst $1) (snd $1) $2 }
    | match Term "{" MatchCases "}" { Match $2 $4 }

Term :: { Expr }
    : small_id { VarExpr $1 }
    | big_id { VarExpr $1 }
    | integer { IntExpr $1 }
    | float { FloatExpr $1 }
    | string { StringExpr $1 }
    | Term Term %prec APP { FuncApp $1 $2 }
    | "(" Expr ")" { $2 }

MatchCases :: { [(Expr, Expr)] }
    : MatchCases MatchCase { ($2:$1) }
    | MatchCase { [$1] }

MatchCase :: { (Expr, Expr) }
    : Term StmtAssignment { ($1, $2) }

Type :: { Type }
    : big_id Types0 %prec APP { TypeCon $1 $2 }
    | "(" Type ")" { $2 }

Types0 :: { [Type] }
    : Types0_ { reverse $1 }

Types0_ :: { [Type] }
    : Types0_ Type { ($2:$1) }
    | {- EMPTY -} { [] }

DataDefinition :: { Data }
    : data big_id "{" Constructors "}" { Data $2 $4 }

-- Order does not matter, so there is no need to use `@reverse@`
Constructors :: { [Constructor] }
    : Constructors Constructor { ($2:$1) }
    | {- EMPTY -} { [] }

Constructor :: { Constructor }
    : big_id "::" Params ";" { Constructor $1 $3 }
    | big_id ";" { Constructor $1 [] }


{
parseError :: Token -> Alex a
parseError tok = do
    posStr <- getPrettyAlexPosn
    alexError $ "Error parsing token on " ++ posStr
        ++ "\n    Unexpected token: `" ++ show tok ++ "`"
}
