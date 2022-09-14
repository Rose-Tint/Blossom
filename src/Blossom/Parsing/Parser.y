{
module Blossom.Parsing.Parser (
    blossomParser,
) where

import Blossom.Parsing.AbsSynTree
import Blossom.Parsing.Lexer
import Blossom.Parsing.Token
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
    import                  { TokImport }
    func                    { TokFunc }
    data                    { TokData }

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

FunctionDefinition :: { Function }
    : func small_id "::" Params "=>" Expr { Function $2 $4 $6 }

Params :: { [Param] }
    : Params_ { reverse $1}

Params_ :: { [Param] }
    : Params_ Param { ($2:$1) }
    | Type { [Param Nothing] }

Param :: { Param }
    : small_id ":" Type { Param (Just $1) }
    | Type { Param Nothing }

Expr :: { Expr }
    : {- ... -} ";" { Expr }

Type :: { () }
    : { () }


{
parseError :: Token -> Alex a
parseError _tok = alexError "Error while parsing"
}
