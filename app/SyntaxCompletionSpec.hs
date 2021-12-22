module SyntaxCompletionSpec where

import SyntaxCompletion (computeCand)
import SynCompInterface
import Test.Hspec

import System.IO (readFile)

spec = hspec $ do
  describe "syntax complection sbparser/app/syntaxcompletion" $ do
    let ex1_sb = "TextWindow."
    it ("[ex1.sb:simple] " ++ ex1_sb) $ do
      results <- computeCand False ex1_sb "" True
      results `shouldBe` [Candidate "white ID white = white ..."]

    it ("[ex1.sb:nested] " ++ ex1_sb) $ do
      results <- computeCand False ex1_sb "" False
      results `shouldBe` [Candidate "white ID white = white ..."]

    let ex2_sb = "IF "
    it ("[ex2.sb:simple] " ++ ex2_sb) $ do
      results <- computeCand False ex2_sb "" True
      results `shouldBe` [Candidate "white ... white Then white ..."]

    it ("[ex2.sb:nested] " ++ ex2_sb) $ do
      results <- computeCand False ex2_sb "" False
      results `shouldBe` [Candidate "white ... white Then white ..."]

    let ex3_sb = "IF x = y Then \n x = (x + y  "
    it ("[ex3.sb:simple] " ++ ex3_sb) $ do
      results <- computeCand True ex3_sb "" True
      results `shouldBe` [Candidate "white )"]

    it ("[ex3.sb:nested] " ++ ex3_sb) $ do
      results <- computeCand True ex3_sb "" False
      results `shouldBe` [Candidate "white )"]


