{
-- {-# LINE 3 "src/Blossom/Package/Parser.y" #-}
{-# OPTIONS_GHC -fno-prof-auto #-}
{-# OPTIONS_GHC -fprof-auto-exported #-}

{-# LANGUAGE RankNTypes #-}

module Blossom.Package.Parser (
    parsePackage,
    parsePackageFile,
) where

import Blossom.Common.Name.Module (ModuleName(..))
import Blossom.Package (Package(..))
import Blossom.Package.Component
import Blossom.Package.Lexer (Alex, lexError, lexer, runLexer)
import Blossom.Package.Token (Token(..))
import Blossom.Package.Properties (Properties(..), emptyProperties)
import Blossom.Package.Target (Target(..))
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy as BS (ByteString, toStrict, readFile)
import Prettyprinter (pretty, (<+>))
}


%name packageParser Package
%error { parseError }
%lexer { lexer } { TokEnd }
%monad { Alex }

%tokentype { Token }
%token
    executable              { TokExeHeader $$ }
    library                 { TokLibHeader $$ }
    subpackage              { TokPkgHeader $$ }
    string                  { TokString $$ }
    version_val             { TokVersionVal $$ }
    ","                     { TokComma }
    "="                     { TokEq }
    "["                     { TokLBracket }
    "]"                     { TokRBracket }
    -- bool                    { TokBool $$ }
    source_dirs             { TokSourceDirs }
    modules                 { TokModules }
    name                    { TokName }
    version                 { TokVersion }
    main                    { TokMain }
    path                    { TokPath }


%%


Package :: { Package }
    : Properties Components { Package $1 $2 }

Properties :: { Properties }
    : Properties Property { $2 $1 }
    | {- EMPTY -} { emptyProperties }

Property :: { Properties -> Properties }
    : name "=" string { \p -> p { propsName = $3 } }
    | version "=" version_val { \p -> p { propsVersion = $3 } }

Components :: { [Component] }
    : Components Component { $2 : $1 }
    | {- EMPTY -} { [] }

Component :: { Component }
    : library LibraryOptions { Library (setCmpntName $1 $2) }
    | executable ExecutableOptions { Executable (setCmpntName $1 $2) }
    | subpackage SubpackageOptions { Subpackage (setCmpntName $1 $2) }

LibraryOptions :: { Library }
    : LibraryOptions LibraryOption { $2 $1 }
    | {- EMPTY -} { newLibrary }

LibraryOption :: { Library -> Library }
    : GeneralOption { $1 }

ExecutableOptions :: { Executable }
    : ExecutableOptions ExecutableOption { $2 $1 }
    | {- EMPTY -} { newExecutable }

ExecutableOption :: { Executable -> Executable }
    : GeneralOption { $1 }
    | main "=" string { \exe -> exe { exeMain = unpack $3 }}

SubpackageOptions :: { Subpackage }
    : SubpackageOptions SubpackageOption { $2 $1 }
    | {- EMPTY -} { newSubpackage }

SubpackageOption :: { Subpackage -> Subpackage }
    : GeneralOption { $1 }
    | path "=" string { \sub -> sub { subPath = unpack $3 }}

GeneralOption :: { forall a. IsCmpnt a => a -> a }
    : source_dirs "=" "[" SourceDirs "]" { setCmpntSourceDirs $4 }
    | modules "=" "[" Targets "]" { setCmpntTargets $4 }

SourceDirs :: { [FilePath] }
    : SourceDirs string "," { unpack $2 : $1 }
    | string "," { [unpack $1] }
    | {- EMPTY -} { [] }

Targets :: { [Target] }
    : Targets string "," { TargetModule (MdlName $2) : $1 }
    | string "," { [TargetModule (MdlName $1)] }
    | {- EMPTY -} { [] }


{
{-# LINE 113 "src/Blossom/Package/Parser.y" #-}

parseError :: Token -> Alex a
parseError tok = lexError $ pretty "Unexpected token:" <+> pretty tok

parsePackage :: ByteString -> Either String Package
parsePackage = flip runLexer packageParser

parsePackageFile :: FilePath -> IO (Either String Package)
parsePackageFile path = do
    contents <- BS.readFile path
    -- the strictness of `contents` is important here!
    let result = parsePackage (contents `seq` contents)
    return result
}
