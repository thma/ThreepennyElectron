module Calc.CalcSpec
  (main, spec)
where

import           Control.Exception     (evaluate)
import           Test.Hspec            
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import           Test.QuickCheck

import           Calc

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = 
  describe "populate" $ do
    it "adds digits to first number" $
      populate "1" initialState  `shouldBe` EnteringA (1.0,False)
    it "adds floating point to first number" $
      populate "." (EnteringA (33.0,False)) `shouldBe` EnteringA (33.0,True)
    it "adds first fractional digit after decimal point" $
      populate "3" (EnteringA (33.0,True)) `shouldBe` EnteringA (33.3,True)
    it "adds second fractional digits after decimal point" $
      populate "3" (EnteringA (3.3,True)) `shouldBe` EnteringA (3.33,True)   
    it "adds second fractional digits after decimal point" $
      populate "3" (EnteringA (33.3,True)) `shouldBe` EnteringA (33.33,True)  
    it "adds second fractional digits after decimal point" $
      populate "3" (EnteringA (333.33,True)) `shouldBe` EnteringA (333.333,True)          
      
      -- property $ \bd -> (fromString . toString) bd === (bd :: BigDecimal)
