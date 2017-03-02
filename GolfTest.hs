module GolfTest where
  import Test.Hspec
  import Test.QuickCheck
  import Control.Exception (evaluate)
  import Golf

  main :: IO ()
  main = hspec $ do

    describe "findEveryNth" $ do
      it "finds every 3th element for a given list" $ do
        findEveryNth [1, 2, 3, 4, 5, 6] 3 `shouldBe` [3, 6]

      it "finds every 2th element for a given list" $ do
        findEveryNth [1, 2, 3, 4, 5, 6] 2 `shouldBe` [2, 4, 6]

    describe "skips" $ do
      it "gives every nth element of a list" $ do
        skips "ABCD" `shouldBe`  ["ABCD", "BD", "C", "D"]
        skips "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
        skips [1] `shouldBe`  [[1]]
        skips [True,False] `shouldBe` [[True,False], [False]]

      it "gives empty list for empty list" $ do
        length (skips []) `shouldBe` 0
