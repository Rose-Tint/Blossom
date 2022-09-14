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

%%

Module :: { AbsSynTree }
    : {- EMPTY -} { AbsSynTree }


{
parseError :: Token -> Alex a
parseError _tok = alexError "Error while parsing"
}
