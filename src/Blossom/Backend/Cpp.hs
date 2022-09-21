{-# LANGUAGE OverloadedStrings #-}

module Blossom.Backend.Cpp (
    transpileFile,
) where

import qualified Data.ByteString.Lazy as BS (readFile)
import qualified Data.ByteString.Char8 as BS (unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS (unpack)
import Data.List (intercalate)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory, replaceExtension)

import Blossom.Common.Name
import Blossom.Parsing.AbsSynTree (
    Constructor(Constructor, ctorName),
    Data(Data),
    Expr(..),
    Param(Param),
    Function(Function),
    TopLevelExpr(..),
    Import(..),
    ModuleAST(ModuleAST) )
import Blossom.Typing.Type (Type(..))
import Blossom.Parsing.Lexer (runAlex)
import Blossom.Parsing.Parser (blossomParser)
import Data.Char (toLower)


transpileFile :: FilePath -> IO ()
transpileFile path = do
    contents <- BS.readFile path
    case runAlex contents blossomParser of
        Left err -> putStrLn $ "Error: " ++ err
        Right ast -> do
            let transpilation = toCpp ast
            let filepath = "./build/cpp/"
                    ++ replaceExtension path ".cpp"
            let dir = takeDirectory filepath
            createDirectoryIfMissing True dir
            writeFile filepath transpilation

indent :: String -> String
indent "" = ""
indent str = "    " ++ go str
    where
        go "" = ""
        go "\n" = "\n"
        go ('\n':cs) = "\n    " ++ go cs
        go (c:cs) = (c:go cs)


class Cpp a where
    toCpp :: a -> String


instance Cpp ModuleAST where
    toCpp (ModuleAST imports defs) =
        "#include <cstdint>\n#include <string>\n\n"
        ++ concatMap toCpp imports
        ++ concatMap toCpp defs
        ++ "\n"

instance Cpp Import where
    toCpp (Import name) = "#include " ++ toCpp name

instance Cpp TopLevelExpr where
    toCpp (FuncDef func) = toCpp func
    toCpp (DataDef dta) = toCpp dta

instance Cpp Function where
    toCpp (Function name params retType body) =
        retType' ++ " " ++ name' ++ "(" ++ params' ++ ") {\n"
        ++ indent body' ++ "\n}"
        where
            retType' = toCpp retType
            name' = toCpp name
            params' = intercalate ", " (map toCpp params)
            body' = toCpp body

instance Cpp Param where
    toCpp (Param name typ) = toCpp typ ++ " " ++ toCpp name

instance Cpp Type where
    -- Primitives:
    toCpp (TypeCon "I64" []) = "std::int64_t"
    toCpp (TypeCon "I32" []) = "std::int32_t"
    toCpp (TypeCon "I16" []) = "std::int16_t"
    toCpp (TypeCon "I8" []) = "std::int8_t"
    toCpp (TypeCon "U64" []) = "std::uint64_t"
    toCpp (TypeCon "U32" []) = "std::uint32_t"
    toCpp (TypeCon "U16" []) = "std::uint16_t"
    toCpp (TypeCon "U8" []) = "std::uint8_t"
    toCpp (TypeCon "F32" []) = "float"
    toCpp (TypeCon "F64" []) = "double"
    toCpp (TypeCon "String" []) = "std::string"
    toCpp (TypeCon "Boolean" []) = "bool"
    toCpp (TypeCon name []) = toCpp name ++ "*"
    toCpp (TypeCon name args) = name' ++ "<" ++ args' ++ ">" ++ "*"
        where
            name' = toCpp name
            args' = intercalate ", " (map toCpp args)
    toCpp (t1 :-> t2) = toCpp t2 ++ " (*)(" ++ toCpp t1 ++ ")"

instance Cpp Data where
    toCpp (Data name ctors) = "struct " ++ toCpp name ++ " {\n"
        ++ indent ctorTagEnum
        ++ indent ctorStructs
        ++ indent ctorsUnion ++ "\n};"
        where
            ctorTags = intercalate ",\n" $ map (toCpp . ctorName) ctors
            ctorTagEnum = "enum {\n" ++ indent ctorTags ++ "\n} ctor_tag;\n"
            ctorStructs = unlines $ map toCpp ctors
            ctorUnionMembers = unlines $ map (\ctor ->
                let structName = toCpp (ctorName ctor)
                    memberName = map toLower structName
                in "Ctor_" ++ structName ++ " " ++ memberName ++ ";"
                ) ctors
            ctorsUnion = "union {\n" ++ indent ctorUnionMembers ++ "};"

instance Cpp Constructor where
    toCpp (Constructor name params) = "struct Ctor_" ++ toCpp name
        ++ " {\n" ++ indent fields ++ "};"
        where
            fields = unlines $ map ((++ ";") . toCpp) params

instance Cpp Expr where
    toCpp (VarExpr name) = toCpp name
    toCpp (IntExpr int) = show int
    toCpp (FloatExpr float) = show float
    toCpp (StringExpr str) = "\"" ++ LBS.unpack str ++ "\""
    -- | assumes that @e1@ is a function type
    toCpp (FuncApp e1 e2) = "(" ++ toCpp e1 ++ ")(" ++ toCpp e2 ++ ")"
    toCpp (Lambda params retType body) = "[](" ++ params' ++ ") -> "
        ++ toCpp retType ++ "{ " ++ toCpp body ++ "}"
        where
            params' = intercalate ", " (map toCpp params)
    toCpp (Match value cases) =
        "auto _match_value = " ++ toCpp value ++ ";\n"
        ++ casesToCpp cases
        where
            casesToCpp [] = ""
            casesToCpp [(case', body)] =
                "if (_match_value.ctor_tag == " ++ toCpp case' ++ ".ctor_tag) {\n"
                ++ indent (toCpp body) ++ "\n}"
            casesToCpp ((case', body):cases') =
                "if (_match_value.ctor_tag == " ++ toCpp case' ++ ".ctor_tag) {\n"
                ++ indent (toCpp body) ++ "\n} else " ++ casesToCpp cases'
        -- error "match cases not yet supported"

instance Cpp Name where
    toCpp (Name name) = BS.unpack name
    toCpp (Id n) = "_" ++ show n
