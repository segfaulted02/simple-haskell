module MiniRacketParserSpec where 

import Test.Hspec
import Parser
import Expr 
import MiniRacketParser
import Error

type ParseResult = Either ErrorT (Expr, String)

expr :: Either ErrorT (a2, b) -> a2
expr (Right (e, _)) = e 
expr (Left (SyntaxError msg)) = error msg
expr (Left (ParseError msg)) = error msg
expr (Left NoParse) = error "no matching parse"
expr _ = error "expr in MiniRacketParser.hs is not fully implemented yet..."

spec :: Spec 
spec = do 
    describe "parse literals" $ do
        it "parses number: 1235" $ 
            parseStr "1235" `shouldBe` Right (LiteralExpr (IntVal 1235),"")
        it "parses negative numbers: -12235" $
            parseStr "-12235" `shouldBe` Right (LiteralExpr (IntVal (-12235)), "")
        it "parses true" $
            parseStr "true" `shouldBe` Right (LiteralExpr (BoolVal True), "")
        it "parses false" $
            parseStr "false" `shouldBe` Right (LiteralExpr (BoolVal False), "")
        it "'not' parses" $
            parseStr "" `shouldBe` Left NoParse
        it "parses and" $
            parse parseBoolOp "and" `shouldBe` Right (And, "")
        it "parses or" $
            parse parseBoolOp "or" `shouldBe` Right (Or, "")
        it "parses <" $
            parse parseCompOp "<" `shouldBe` Right (Lt, "")
        it "parses equal?" $
            parse parseCompOp "equal?" `shouldBe` Right (Eq, "")
        it "parses *" $
            parse parseMathOp "*" `shouldBe` Right (Mul, "")
        it "parses div" $
            parse parseMathOp "`div`" `shouldBe` Right (Div, "")
        it "parses mod" $
            parse parseMathOp "`mod`" `shouldBe` Right (Mod, "")
        it "parses a math expression" $
            parse parseExpr "(* 5 5)" `shouldBe` Right (MathExpr Mul [LiteralExpr (IntVal 5),LiteralExpr (IntVal 5)],"")