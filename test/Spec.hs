import Test.Hspec
import Lib

-- do describe do it do function should be assert
main :: IO ()
main = hspec $ do describe "How to write a test" $ do it "Should return a string Hello world" $ do myFunction `shouldBe` "Hello World"
