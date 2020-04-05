import Test.Hspec
import Lib

-- do describe do it do function should be assert
main :: IO ()
main = hspec $ do
    describe "How to write a test using the myFuction test function" $ do
        it "Should return a string Hello world" $ do
            myFunction `shouldBe` "Hello World"

    describe "The function format grid is supposed to append newlines to a list of strings and return a string" $ do
        it "Should return\n SARS\n MERS\n SAIL\n BAIL" $ do
            (formatGrid ["SARS", "MERS", "SAIL", "BAIL"]) `shouldBe` "SARS\n MERS\n SAIL\n BAIL\n"