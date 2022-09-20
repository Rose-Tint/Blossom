module Blossom.Backend.Cpp (
    transpileFile,
) where

import qualified Data.ByteString.Char8 as BS (unpack)
import Data.List (intercalate)

import Blossom.Common.Name
import Blossom.Parsing.AbsSynTree (
    Constructor(Constructor, ctorName),
    Data(Data),
    Expr(Match, VarExpr, IntExpr, FloatExpr, FuncApp, Lambda),
    Param(Param),
    Function(Function),
    TopLevelExpr(..),
    Import(..),
    ModuleAST(ModuleAST) )
import Blossom.Typing.Type (Type(..))
import Blossom.Parsing.Lexer (runAlex)
import Blossom.Parsing.Parser (blossomParser)
import qualified Data.ByteString.Lazy as ByteString


transpileFile :: FilePath -> IO ()
transpileFile path = do
    contents <- ByteString.readFile path
    case runAlex contents blossomParser of
        Left err -> putStrLn $ "Error: " ++ err
        Right ast -> do
            let transpilation = toCpp ast
            let newPath = "./build/cpp/" ++ path
            writeFile newPath transpilation

indent :: String -> String
indent str = "    " ++ go str
    where
        go "" = ""
        go ('\n':cs) = "\n    " ++ go cs
        go (c:cs) = (c:go cs)


class Cpp a where
    toCpp :: a -> String


instance Cpp ModuleAST where
    toCpp (ModuleAST imports defs) =
        concatMap toCpp imports
        ++ concatMap toCpp defs

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
    toCpp (TypeCon name []) = toCpp name
    toCpp (TypeCon name args) = name' ++ "<" ++ args' ++ ">"
        where
            name' = toCpp name
            args' = intercalate ", " (map toCpp args)
    toCpp (t1 :-> t2) = toCpp t2 ++ " (*)(" ++ toCpp t1 ++ ")"

instance Cpp Data where
    toCpp (Data name ctors) = "struct " ++ toCpp name ++ " {\n"
        ++ indent ctorTagEnum ++ indent ctorsUnion ++ "\n};"
        where
            ctorTags = unlines $ map (toCpp . ctorName) ctors
            ctorTagEnum = "enum {\n" ++ indent ctorTags ++ "\n} ctor_tag;"
            ctorStructs = unlines $ map toCpp ctors
            ctorsUnion = "union {\n" ++ indent ctorStructs ++ "\n};"

instance Cpp Constructor where
    toCpp (Constructor name params) = "struct " ++ toCpp name
        ++ " {\n" ++ indent fields ++ "\n};"
        where
            fields = unlines $ map ((++ ";") . toCpp) params

instance Cpp Expr where
    toCpp (VarExpr name) = toCpp name
    toCpp (IntExpr int) = show int
    toCpp (FloatExpr float) = show float
    -- | assumes that @e1@ is a function type
    toCpp (FuncApp e1 e2) = "(" ++ toCpp e1 ++ ")(" ++ toCpp e2 ++ ")"
    toCpp (Lambda params retType body) = "[](" ++ params' ++ ") -> "
        ++ toCpp retType ++ "{ " ++ toCpp body ++ "}"
        where
            params' = intercalate ", " (map toCpp params)
    toCpp (Match _value _cases) = error "match cases not yet supported"

instance Cpp Name where
    toCpp (Name name) = BS.unpack name
    toCpp (Id n) = "_" ++ show n
