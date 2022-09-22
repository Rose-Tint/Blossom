{
module Blossom.Parsing.Parser (
    parse,
) where

import Blossom.Common.Name (Name)
import Blossom.Parsing.AbsSynTree (
    ModuleAST(..),
    Import(..),
    TopLevelExpr(..),
    Pattern(..),
    Expr(..),
    Constructor(..),
    Case(..),
    Params,
    )
import Blossom.Parsing.Lexer (
    Alex,
    runAlex,
    lexer,
    getPrettyAlexPosn,
    alexError,
    )
import Blossom.Parsing.Token (Token(..))
import Blossom.Typing.Type (Type(..))
import Data.ByteString.Lazy (ByteString)
}


%name blossomParser Module
%error { parseError }
%lexer { lexer } { TokEnd }
%monad { Alex }
%expect 4

%tokentype { Token }
%token
    integer                 { TokInteger $$ }
    float                   { TokFloat $$ }
    char                    { TokChar $$ }
    string                  { TokString $$ }
    operator                { TokOperator $$ }
    small_id                { TokSmallId $$ }
    big_id                  { TokBigId $$ }
    "\\"                    { TokBackslash }
    ";"                     { TokSemi }
    ":"                     { TokColon }
    -- "::"                    { TokDoubleColon }
    "->"                    { TokArrow }
    "="                     { TokEquals }
    "=>"                    { TokEqArrow }
    "("                     { TokLParen }
    ")"                     { TokRParen }
    "{"                     { TokLBrace }
    "}"                     { TokRBrace }
    import                  { TokImport }
    func                    { TokFunc }
    data                    { TokData }
    match                   { TokMatch }

-- PLEASE NOTE: If function application is not as expected,
-- please remove the following precedence declaration.
-- It did turn 12 s/r conflicts into reduces for FuncApp.
-- (commented out bc it messed things up. :/)
-- %nonassoc integer float string small_id big_id char "(" -- "->"

-- ghost precedence for function application
-- https://stackoverflow.com/questions/27630269
-- %nonassoc APP


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
    : FuncDecl { $1}
    | FuncDef { $1 }
    | DataDefinition { $1 }

-- | The part that prefixes both function
-- declarations *and* definitions
-- TODO: abstract `small_id` and `operator` to one.
FuncOpener :: { Name }
    : func small_id { $2 }
    | func "(" operator ")" { $3 }

FuncDecl :: { TopLevelExpr }
    : FuncOpener SigWithSemi { FuncDecl $1 $2 }

FuncDef :: { TopLevelExpr }
    : FuncOpener Params StmtAssignment { FuncDef $1 $2 $3 }

Signature :: { Type }
    : ":" Type { $2 }

-- | A type-signature that ends with a semicolon
SigWithSemi :: { Type }
    : Signature ";" { $1 }

Params :: { Params }
    : Params_ { reverse $1}

Params_ :: { Params }
    : Params_ Pattern { ($2:$1) }
    | Pattern { [$1] }

Pattern :: { Pattern }
    : small_id { Param $1 }
    | "(" big_id Params ")" { CtorPtrn $2 $3 }

Stmt :: { Expr }
    : Expr ";" { $1 }

StmtAssignment :: { Expr }
    : "=" Stmt { $2 }

Implication :: { Expr }
    : "=>" Expr { $2 }

Expr :: { Expr }
    : Term { $1 }
    | Term Signature { TypedExpr $1 $2 }
    | "\\" Params Implication { Lambda $2 $3 }
    | match Term "{" MatchCases "}" { Match $2 $4 }
    | FuncApp { FuncApp $1 }

Term :: { Expr }
    : small_id { VarExpr $1 }
    | big_id { VarExpr $1 }
    | integer { IntExpr $1 }
    | float { FloatExpr $1 }
    | char { CharExpr $1 }
    | string { StringExpr $1 }
    | operator { VarExpr $1 }
    -- | "(" operator ")" { VarExpr $2 }
    | "(" Expr ")" { $2 }

FuncApp :: { [Expr] }
    : FuncApp_ { reverse $1 }

FuncApp_ :: { [Expr] }
    : FuncApp_ Term { ($2:$1) }
    -- | FuncApp_ operator { (VarExpr $2:$1) }
    | Term Term { [$2, $1] } -- backwards bc it gets reversed

MatchCases :: { [Case] }
    : MatchCases MatchCase { ($2:$1) }
    | MatchCase { [$1] }

MatchCase :: { Case }
    : Pattern Implication ";" { Case $1 $2 }

Type :: { Type }
    : big_id Types0 { TypeCon $1 $2 }
    | Type "->" Type { $1 :-> $3 }
    | "(" Type ")" { $2 }

Types0 :: { [Type] }
    : Types0_ { reverse $1 }

Types0_ :: { [Type] }
    : Types0_ Type { ($2:$1) }
    | {- EMPTY -} { [] }

DataDefinition :: { TopLevelExpr }
    : data big_id "{" Constructors "}" { DataDef $2 $4 }

-- Order does not matter, so there is no need to use `@reverse@`
Constructors :: { [Constructor] }
    : Constructors Constructor { ($2:$1) }
    | {- EMPTY -} { [] }

Constructor :: { Constructor }
    : big_id SigWithSemi { Constructor $1 $2 }
    | big_id ";" { Nullary $1 }


{
parseError :: Token -> Alex a
parseError tok = do
    posStr <- getPrettyAlexPosn
    alexError $ "Error parsing token on " ++ posStr
        ++ "\n    Unexpected token: `" ++ show tok ++ "`"

parse :: ByteString -> Either String ModuleAST
parse = flip runAlex blossomParser
}
