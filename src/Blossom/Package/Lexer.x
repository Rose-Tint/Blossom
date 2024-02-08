{
{-# LINE 3 "src/Blossom/Package/Lexer.x" #-}
-- Suppresses warning about a redundant import generated by Alex
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -fno-prof-auto #-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Blossom.Package.Lexer (
    Alex(..),
    runLexer,
    lexError,
    lexer,
    tokenize,
) where

import Blossom.Common.Name (Ident(..))
import Blossom.Common.Source (Position(..), SourceLoc(..), mkPos)
import Blossom.Package.Token (Token(..))
import Blossom.Package.Version (Version(..))
import Data.ByteString.Lazy.Char8 (ByteString, unpack)
import qualified Data.ByteString.Lazy.Char8 as CBS (
    foldl',
    span,
    break,
    dropWhile,
    )
-- import qualified Data.ByteString.Lazy as ByteString -- imported by Alex
import Data.Char (isDigit, digitToInt)
import Prettyprinter (Doc, Pretty(..), nest, (<+>))
import qualified Prettyprinter as P (line)

}

%wrapper "monadUserState-bytestring"

$digit              = [0-9]
$ws                 = [\x09 \x20]
$bare_key_char      = [A-Za-z0-9\_\-]
$escape_char        = [b t n f r \" \\]

@version            = $digit+(\.$digit(\.$digit)?)?
@newline            = (\x0D? \x0A)+
@section_name       = $bare_key_char+ (\. $bare_key_char+)*
@string_char        = [^\"] | \\$escape_char

tokens :-
    $ws                             { skip }
    @newline                        { begin 0 }
    <sc_list> $white                { skip }
    "#" .*                          { begin 0 }

    <sc_value> \"                   { begin sc_string }
    <sc_string> @string_char+       { mkString }
    <sc_string> \"                  { begin sc_value }

    <0> "["                         { begin sc_sect }
    <sc_sect> @section_name         { section }
    <sc_sect> "]"                   { begin 0 }

    -- keys
    <0> "source-dirs"               { reserved KeySourceDirs }
    <0> "targets"                   { reserved KeyTargets }
    <0> "name"                      { reserved KeyName }
    <0> "version"                   { reserved KeyVersion }
    <0> "main"                      { reserved KeyMain }
    <0> "="                         { reserved TokEq `andBegin` sc_value }
    <sc_value> @version             { version }
    <sc_value> "["                  { lBracket }
    <sc_list> "]"                   { reserved TokRBracket `andBegin` 0 }
    <sc_list> ","                   { reserved TokComma }


{
{-# LINE 77 "src/Blossom/Package/Lexer.x" #-}

alexEOF :: Alex Token
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
--     Int64                     -- bytes consumed so far
--     )

type AlexUserState = FilePath

-- AlexUserState initialization function
alexInitUserState :: AlexUserState
alexInitUserState = "<package-name?>.toml"

posnToPos :: AlexPosn -> Position
posnToPos (AlexPn off ln col) = mkPos ln col off

-- | Creates a `@SourceLoc@`. The second parameter is the length of the token
-- that the `@SourceLoc@` is for.
posnToLoc :: AlexPosn -> Int64 -> Alex SourceLoc
posnToLoc posn len = do
    let end = posnToPos posn
    let start = end { posOffset = posOffset end - fromIntegral len }
    return $ SourceLoc mempty mempty start end

getSourceLoc :: Alex SourceLoc
getSourceLoc = do
    (posn, _, _, _) <- alexGetInput
    loc <- posnToLoc posn 0
    return loc

type Action = AlexInput -> Int64 -> Alex Token

mkString :: Action
mkString (_, _, input, _) len = return $ TokString $
    ByteString.toStrict $ ByteString.take len input

section :: Action
section (_, _, input, _) len = case ByteString.take len input of
    "executable" -> return TokExeHeader
    "library" -> return TokLibHeader
    str -> lexError $ "unrecognized component:" <+> pretty (unpack str)

reserved :: Token -> Action
reserved tok _ _ = return tok

equal :: Action
equal = reserved TokEq `andBegin` sc_value

lBracket :: Action
lBracket = reserved TokLBracket `andBegin` sc_list

rBracket :: Action
rBracket = reserved TokRBracket `andBegin` sc_value

version :: Action
version (_, _, input, _) len = do
    vers <- case readNum (ByteString.take len input) of
        Nothing -> lexError "Could not read version"
        Just (major, rest) -> case readNum (dropDot rest) of
            Nothing -> return (Version major 0 0)
            Just (minor, rest') -> case readNum (dropDot rest') of
                Nothing -> return (Version major minor 0)
                Just (patch, _) -> return (Version major minor patch)
    return (TokVersion vers)
    where
        dropDot = CBS.dropWhile (== '.')
        readNum str =
            let (numStr, rest) = CBS.span isDigit (dropDot str)
            in if ByteString.null numStr then
                Nothing
            else
                let num = CBS.foldl' (\n ch ->
                        n * 10 + fromIntegral (digitToInt ch)
                        ) 0 numStr
                in Just (num, rest)

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

-- | Turns a bytestring into either an error message (`@Left@`), or
-- a list of tokens. This should really only be used for testing.
tokenize :: ByteString -> Either String [Token]
tokenize = flip runLexer go
    where
        go = alexMonadScan >>= \case
            TokEnd -> return []
            tok -> (tok:) <$> go

runLexer :: ByteString -> Alex a -> Either String a
runLexer = runAlex

lexError :: Doc ann -> Alex a
lexError doc = do
    loc <- getSourceLoc
    alexError $ show $ pretty loc <> ": Error:" <> nest 4 (P.line <> doc)
}
