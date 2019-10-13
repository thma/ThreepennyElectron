module Calc.CalcSpec
  (main, spec)
where

import           Control.Exception     (evaluate)
import           Test.Hspec            
import           Test.Hspec.QuickCheck (modifyMaxSize, modifyMaxSuccess)
import           Test.QuickCheck

import           Calc
import           Data.BigDecimal

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

-- | keySequence is expected to be a string with blank between valid keynames. eg "1 2 + 3 4 ="
compute :: String -> State
compute keySequence = foldr populate initialState (words . reverse $ keySequence)

spec :: Spec
spec = 
  describe "populate" $ do
    it "adds digits to first number" $
      populate "1" initialState  `shouldBe` EnteringA ("01",False)
    it "adds floating point to first number" $
      populate "." (EnteringA ("33",False)) `shouldBe` EnteringA ("33.",True)
    it "adds first fractional digit after decimal point" $
      populate "3" (EnteringA ("33.",True)) `shouldBe` EnteringA ("33.3",True)
    it "adds second fractional digits after decimal point" $
      populate "3" (EnteringA ("3.3",True)) `shouldBe` EnteringA ("3.33",True)   
    it "adds second fractional digits after decimal point" $
      populate "3" (EnteringA ("33.3",True)) `shouldBe` EnteringA  ("33.33",True)  
    it "adds second fractional digits after decimal point" $
      populate "3" (EnteringA ("333.33",True)) `shouldBe` EnteringA ("333.333",True)          
    it "adds zeros as fractional digits" $
      populate "0" (EnteringA ("333.33",True)) `shouldBe` EnteringA ("333.330",True)
    it "maintains zeros as fractional digits" $
      populate "3" (populate "0" (EnteringA ("333.33",True))) `shouldBe` EnteringA ("333.3303",True)   
    it "can add numbers" $
      display (compute "1 2 + 8 8 =") `shouldBe` "100"
    it "can multiply numbers" $
      display (compute "1 . 2 * 1 . 2 =") `shouldBe` "1.44"   
    it "can substract numbers" $
      display (compute "1 . 2 - 1 . 2 =") `shouldBe` "0.0"   
    it "can divide numbers" $
      display (compute "2 7 / 3 =") `shouldBe` "9.0"   
    it "gives an error on division by zero" $
      display (compute "2 7 / 0 =") `shouldBe` "Division by Zero!"     
    it "computes sequences of operation" $
      display (compute "2 + 2 + 2 + 2 * 8 = =") `shouldBe` "512.0"        
      -- property $ \bd -> (fromString . toString) bd === (bd :: BigDecimal)
--}