{-# LANGUAGE OverloadedStrings #-}

module Blossom.Parsing.ParserTest (
    tests,
) where

import Test.HUnit (Test(..), (@=?))
import Blossom.Parsing.AbsSynTree (
    ModuleAST(..),
    Import(..),
    TopLevelExpr(..),
    Pattern(..),
    Expr(..),
    Constructor(..),
    )
import Blossom.Parsing.Parser (parse)
import Blossom.Typing.Type (Type(..))


tests :: Test
tests = TestLabel "Blossom.Parsing.Parser" $ TestList [
    TestCase $
        let actual = parse
                "import Pretty;\n\
                \data ExceptI32 {\n\
                \    Failure : String;\n\
                \    Success : I32;\n\
                \}\n\
                \func fail : String -> ExceptI32 -> ExceptI32;\n\
                \func fail newMsg (Failure oldMsg)\n\
                \    = Failure (oldMsg ++ \"\\n~~and\\n\" ++ newMsg);\n\
                \func fail msg (Success i)\n\
                \    = Failure (msg ++ \"\\nLast value: \" ++ pretty i);\n"
            expected = Right $ ModuleAST {
                moduleImports = [Import "Pretty"],
                moduleTopExprs = reverse [
                    DataDef "ExceptI32" (reverse [
                        Constructor "Failure" (TypeCon "String" []),
                        Constructor "Success" (TypeCon "I32" [])
                        ]),
                    FuncDecl "fail" (
                        TypeCon "String" []
                        :-> TypeCon "ExceptI32" []
                        :-> TypeCon "ExceptI32" []
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
                                StringExpr "\\n~~and\\n",
                                VarExpr "++",
                                VarExpr "newMsg"
                                ]
                            ]
                        ),
                    FuncDef "fail"
                        -- params
                        [Param "msg", CtorPtrn "Success" [Param "i"]]
                        -- body
                        (FuncApp [
                            VarExpr "Failure",
                            FuncApp [
                                VarExpr "msg",
                                VarExpr "++",
                                StringExpr "\\nLast value: ",
                                VarExpr "++",
                                VarExpr "pretty",
                                VarExpr "i"
                                ]
                            ]
                        )
                    ]
                }
        in expected @=? actual
    ]
