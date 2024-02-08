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
import Blossom.Package.Package (Package(..), newPackage)
import Blossom.Package.Component
import Blossom.Package.Lexer (Alex, lexError, lexer, runLexer)
import Blossom.Package.Token (Token(..))
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
    executable              { TokExeHeader }
    library                 { TokLibHeader }
    string                  { TokString $$ }
    version_val             { TokVersion $$ }
    ","                     { TokComma }
    "="                     { TokEq }
    "["                     { TokLBracket }
    "]"                     { TokRBracket }
    source_dirs             { KeySourceDirs }
    targets                 { KeyTargets }
    name                    { KeyName }
    version                 { KeyVersion }
    main                    { KeyMain }

%%


Package :: { Package }
    : Properties Components { $1 { pkgComponents = $2 } }

Properties :: { Package }
    : Properties PackageProperty { $2 $1 }
    | {- EMPTY -} { newPackage }

PackageProperty :: { Package -> Package }
    : name "=" string { \p -> p { pkgName = $3 } }
    | version "=" version_val { \p -> p { pkgVersion = $3 } }

Components :: { [Component] }
    : Components Component { $2 : $1 }
    | {- EMPTY -} { [] }

Component :: { Component }
    : library LibraryOptions { $2 }
    | executable ExecutableOptions { $2 }

LibraryOptions :: { Component }
    : LibraryOptions LibraryOption { $2 $1 }
    | {- EMPTY -} { newLibrary }

LibraryOption :: { Component -> Component }
    : GeneralOption { $1 }

ExecutableOptions :: { Component }
    : ExecutableOptions ExecutableOption { $2 $1 }
    | {- EMPTY -} { newExecutable }

ExecutableOption :: { Component -> Component }
    : GeneralOption { $1 }
    | main "=" string { \exe -> exe { exeMain = unpack $3 }}

-- an option that may appear in any component
GeneralOption :: { Component -> Component }
    : name "=" string { \c -> c { cmpntName = $3 } }
    | source_dirs "=" SourceDirList { \c -> c { cmpntSourceDirs = $3 } }
    | targets "=" TargetList { \c -> c { cmpntTargets = $3 } }

SourceDirList :: { [FilePath] }
    : "[" SourceDirs "," "]" { $2 }
    | "[" SourceDirs "]" { $2 }
    | "[" "]" { [] }

SourceDirs :: { [FilePath] }
    : SourceDirs "," string { unpack $3 : $1 }
    | string { [unpack $1] }

TargetList :: { [Target] }
    : "[" Targets "," "]" { $2 }
    | "[" Targets "]" { $2 }
    | "[" "]" { [] }

Targets :: { [Target] }
    : Targets "," Target { $3 : $1 }
    | Target { [$1] }

Target :: { Target }
    : string { TargetModule (MdlName $1) }


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
