import Test.Hspec
import Lib

-- The testing pattern with hspec is:
-- $ do describe <descibe test> $ do it <what should the function do> $ do <run function with args> `shouldBe` <assertion value>

testGrid :: [String]
testGrid = ["***WORD****"
           ,"**I**U****C"
           ,"LL***N****A"
           ,"LL****E***R"
           ,"*AA****E***"
           ,"*C*B****D**"
           ,"***********"
           ,"**TSIL*****"
           ]

testSearchWords :: [String]
testSearchWords = ["WORD"
                  ,"WILL"
                  ,"CAR"
                  ,"CALL"
                  ,"NEED"
                  ,"LIST"
                  ,"BALL"
                  ,"RUN"
                  ]

main :: IO ()
main = hspec $ do
    describe "How to write a test using the myFuction test function" $ do
        it "Should return a string Hello world" $ do
            myFunction `shouldBe` "Hello World"

    describe "The function format grid is supposed to append newlines to a list of strings and return a string" $ do
        it "Should return\n SARS\n MERS\n SAIL\n BAIL" $ do
            (formatGrid ["SARS", "MERS", "SAIL", "BAIL"]) `shouldBe` "SARS\n MERS\n SAIL\n BAIL\n"

    describe "The function anotherFormatter is supposed to append newlines to a list of strings and return a string" $ do
        it "Should return\n SARS\n MERS\n SAIL\n BAIL" $ do
            (anotherFormatter "\n" ["SARS", "MERS", "SAIL", "BAIL"]) `shouldBe` "SARS\nMERS\nSAIL\nBAIL"

    describe "The check for words complete is supposed to transform and transpose the grid to search and return the words" $ do
        it "Should return\n[WORD, WILL, CAR, CALL, NEED, LIST, BALL]" $ do
            (checkForWordsGeneric testGrid testSearchWords) `shouldBe` ["WORD","LIST","CAR","CALL","RUN","WILL","NEED","BALL"]