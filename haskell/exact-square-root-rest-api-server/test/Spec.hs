import ExactRoot (getExactSqrt)
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "getExactSqrt" $ do
    it "sqrt from 0" $ do
      getExactSqrt 0 `shouldBe` [(-1, 0)]

    it "sqrt from 1" $ do
      getExactSqrt 1 `shouldBe` [(-1, 1)]
    
    it "sqrt from 2" $ do
      getExactSqrt 2 `shouldBe` []
    
    it "sqrt from 3" $ do
      getExactSqrt 3 `shouldBe` []
    
    it "sqrt from 4" $ do
      getExactSqrt 4 `shouldBe` [(-1, 2)]

    it "sqrt from 5" $ do
      getExactSqrt 5 `shouldBe` []
    
    it "sqrt from 6" $ do
      getExactSqrt 6 `shouldBe` []

    it "sqrt from 7" $ do
      getExactSqrt 7 `shouldBe` []

    it "sqrt from 8" $ do
      getExactSqrt 8 `shouldBe` [(2, 2)]

    it "sqrt from 9" $ do
      getExactSqrt 9 `shouldBe` [(-1, 3)]

    it "sqrt from 10" $ do
      getExactSqrt 10 `shouldBe` []

    it "sqrt from 11" $ do
      getExactSqrt 11 `shouldBe` []

    it "sqrt from 12" $ do
      getExactSqrt 12 `shouldBe` [(2, 3)]

    it "sqrt from 13" $ do
      getExactSqrt 13 `shouldBe` []

    it "sqrt from 25" $ do
      getExactSqrt 25 `shouldBe` [(-1, 5)]

    it "sqrt from 50" $ do
      getExactSqrt 50 `shouldBe` [(5, 2)]

    it "sqrt from 1000" $ do
      getExactSqrt 1000 `shouldBe` [(2, 250) ,(5, 40) ,(10, 10)]

    it "sqrt from 123956844" $ do
      getExactSqrt 123956844 `shouldBe` [(2, 30989211)]
    
    -- it "setSqrtAndRadicand" $ do
    --   getSqrtAndRadicand 25 `shouldBe` [(2, 4), (3, 9), (4, 16), (5, 25)]