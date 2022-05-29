module EvalSpec where


import Test.Hspec
import Parser
import Expr
import MiniRacketParser

import Eval
import Error

type ParseResult = Either ErrorT (Expr, String)

spec :: Spec
spec = do
    describe "eval expressions" $ do
        it "evaluates number: 1235" $ 
            evalStr "1235" `shouldBe` Right (IntVal 1235)
        it "evaluates negative numbers: -12235" $
            evalStr "-12235" `shouldBe` Right (IntVal (-12235))
        it "evaluates true" $
            evalStr "true" `shouldBe` Right (BoolVal True)
        it "evaluates false" $
            evalStr "false" `shouldBe` Right (BoolVal False)
        it "evaluates and" $
            evalStr "(and true (and false true) true)" `shouldBe` Right (BoolVal False)
        it "evaluates or" $
            evalStr "(or true (or false true) true)" `shouldBe` Right (BoolVal True)
        it "evaluates not" $
            evalStr "" `shouldBe` Left NoParse
        it "evaluates <" $
            evalStr "(< 16 15)" `shouldBe` Right (BoolVal False)
        it "evaluates equal?" $
            evalStr "(equal? 15 15)" `shouldBe` Right (BoolVal True)
        it "evaluates +" $
            evalStr "(+ 5 5)" `shouldBe` Right (IntVal 10)
        it "evaluates -" $
            evalStr "(- 5 5)" `shouldBe` Right (IntVal 0)
        it "evaluates *" $
            evalStr "(* 5 5)" `shouldBe` Right (IntVal 25)
        it "evaluates div" $
            evalStr "(`div` 5 5)" `shouldBe` Right (IntVal 1)
        it "evaluates mod" $
            evalStr "(`mod` 5 5)" `shouldBe` Right (IntVal 0)