{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Blossom.Parsing.ParserTest (
    tests,
) where

import Test.HUnit (Test(..), (@=?))
import Blossom.Common.Literal (Literal(StringLit))
import Blossom.Common.Name.Ident (Ident, testIdent)
import Blossom.Parsing.Parser (parseModule, parseType)
import Blossom.Parsing.SynTree
import Blossom.Typing.Type (Type(..))
import Data.String (IsString(fromString))


tests :: Test
tests = TestLabel "Blossom.Parsing.Parser" $ TestList [
    TestLabel "types" $ TestList [
        TestCase $
            let actual = parseType "String -> List a" "" ""
                expected = Right $
                    TypeCon "String" :-> TypeApp (TypeCon "List") (TypeVar "a")
            in expected @=? actual,
        TestCase $
            let actual = parseType "(a -> b -> b) -> List a -> b -> b" "" ""
                expected = Right $
                    (TypeVar "a" :-> TypeVar "b" :-> TypeVar "b")
                    :-> TypeApp (TypeCon "List") (TypeVar "a")
                    :-> TypeVar "b"
                    :-> TypeVar "b"
            in expected @=? actual,
        TestCase $
            let actual = parseType "Map k v" "" ""
                expected = Right $
                    TypeApp (TypeApp (TypeCon "Map") (TypeVar "k")) (TypeVar "v")
            in expected @=? actual
        ],
    TestLabel "module" $ TestCase $
        let actual = parseModule
                "import Text::Pretty;\n\
                \data Except a {\n\
                \    Failure : String;\n\
                \    Success : a;\n\
                \}\n\
                \func fail : String -> Except a -> Except a;\n\
                \func fail newMsg (Failure oldMsg)\n\
                \    = Failure (oldMsg ++ \"\\n~~and\\n\" ++ newMsg);\n\
                \func fail msg (Success a)\n\
                \    = Failure (msg ++ \"\\nLast value: \" ++ pretty a);\n"
                "" ""
            expected = Right $ SynTree {
                moduleImports = [Import "Text::Pretty"],
                moduleTopExprs = reverse [
                    DataDef "Except" ["a"] (reverse [
                        Constructor "Failure" (TypeCon "String"),
                        Constructor "Success" (TypeVar "a")
                        ]),
                    FuncDecl "fail" (
                        TypeCon "String"
                        :-> TypeApp (TypeCon "Except") (TypeVar "a")
                        :-> TypeApp (TypeCon "Except") (TypeVar "a")
                        ),
                    FuncDef "fail"
                        -- params
                        [Param "newMsg", CtorPtrn "Failure" [Param "oldMsg"]]
                        -- body
                        (FuncApp [
                            VarExpr "Failure",
                            FuncApp [
                                VarExpr "oldMsg",
                                VarExpr "++",
                                LitExpr (StringLit "\\n~~and\\n"),
                                VarExpr "++",
                                VarExpr "newMsg"
                                ]
                            ]
                        ),
                    FuncDef "fail"
                        -- params
                        [Param "msg", CtorPtrn "Success" [Param "a"]]
                        -- body
                        (FuncApp [
                            VarExpr "Failure",
                            FuncApp [
                                VarExpr "msg",
                                VarExpr "++",
                                LitExpr (StringLit "\\nLast value: "),
                                VarExpr "++",
                                VarExpr "pretty",
                                VarExpr "a"
                                ]
                            ]
                        )
                    ]
                }
        in expected @=? actual
    ]

-- | orphan instance for testing purposes only
instance IsString Ident where
    fromString = testIdent
