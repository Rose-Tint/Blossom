{
-- Suppresses warning about a redundant import generated by Alex
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Blossom.Parsing.Lexer (
    Alex(..),
    runAlex,
    alexError,
    lexer,
    tokenize,
) where

import Data.Char (digitToInt)
import qualified Data.ByteString.Lazy.Char8 as CharByteString
import qualified Data.ByteString.Short as ShortByteString
-- import qualified Data.ByteString.Lazy as ByteString -- imported by Alex
import qualified LLVM.AST.Name as LLVM

import Blossom.Parsing.Token
}

%wrapper "monadUserState-bytestring"

$digit              = [0-9]
$small              = [a-z\_]
$big                = [A-Z]
$id_char            = [a-zA-Z0-9\_\']

@comment            = "--" .*
@integer            = $digit+
@float              = $digit+ \. $digit+
@string             = \" [^\"]* \"
@small_id           = $small $id_char*
@big_id             = $big $id_char*

tokens :-
    $white+                 { skip }
    @comment+               { skip }
    @integer                { integer }
    @float                  { float }
    @string                 { string }
    @small_id               { smallId }
    @big_id                 { bigId }
    ";"                     { reserved TokSemi }
    ":"                     { reserved TokColon }
    "::"                    { reserved TokDoubleColon }
    "->"                    { reserved TokArrow }
    "=>"                    { reserved TokEqArrow }
    "("                     { reserved TokLParen }
    ")"                     { reserved TokRParen }
    "{"                     { reserved TokLBrace }
    "}"                     { reserved TokRBrace }
    "import"                { reserved TokImport }
    "func"                  { reserved TokFunc }
    "data"                  { reserved TokData }
    "match"                 { reserved TokMatch }


-- this lets the error messaging give a the proper position
{alexEOF :: Alex Token
alexEOF = return TokEnd


-- data AlexState = AlexState {
--         alex_pos :: !AlexPosn,  -- position at current input location
--         alex_inp :: ByteString.ByteString, -- the current input
--         alex_chr :: !Char,      -- the character before the input
--         alex_scd :: !Int        -- the current startcode
--       , alex_ust :: AlexUserState -- AlexUserState will be defined in the user program
--     }

-- type AlexInput = (
--     AlexPosn,                 -- current position,
--     Char,                     -- previous char
--     ByteString.ByteString,    -- current input string
--     Int64                     -- ?
--     )

data AlexUserState = AlexUserState

-- AlexUserState initialization function
alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState


integer :: AlexInput -> Int64 -> Alex Token
integer (_pos, _prev, input, _) len = return $ TokInteger $
    CharByteString.foldl' (\n ch ->
        n * 10 + fromIntegral (digitToInt ch)
        ) 0 (ByteString.take len input)

float :: AlexInput -> Int64 -> Alex Token
float (_pos, _prev, input, _) len = return $ TokFloat $
    read (CharByteString.unpack (ByteString.take len input))

string :: AlexInput -> Int64 -> Alex Token
string (_pos, _prev, input, _) len = return $ TokString $
    -- drop quotation marks
    ByteString.drop 1 (ByteString.take (len - 1) input)

smallId :: AlexInput -> Int64 -> Alex Token
smallId (_pos, _prev, input, _) len = return $ TokSmallId $
    LLVM.mkName (CharByteString.unpack (ByteString.take len input))

bigId :: AlexInput -> Int64 -> Alex Token
bigId (_pos, _prev, input, _) len = return $ TokBigId $
    LLVM.mkName (CharByteString.unpack (ByteString.take len input))

reserved :: Token -> AlexInput -> Int64 -> Alex Token
reserved tok (_pos, _prev, _input, _) _len = return tok

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

-- | Turns a bytestring into either an error message (`@Left@`), or
-- a list of tokens.
tokenize :: ByteString.ByteString -> Either String [Token]
tokenize bs = runAlex bs tokenizer
    where
        tokenizer :: Alex [Token]
        tokenizer = do
            tok <- alexMonadScan
            if tok == TokEnd then
                return []
            else do
                toks <- tokenizer
                return (tok:toks)
}
