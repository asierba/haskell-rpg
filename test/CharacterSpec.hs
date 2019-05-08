module CharacterSpec where

import Test.Hspec
import Character
import Character (Health(..), Level(..), Damage(..))

spec :: Spec
spec = do
  describe "Character" $ do
    describe "create" $ do
      it "creates a character with health at 1000" $ do
        health create `shouldBe` Health 1000
      it "creates a character with a level 1" $ do
        level create `shouldBe` Level 1
      it "creates a character alive" $ do
        isAlive create `shouldBe` True
    describe "damage" $ do
      it "is subtracted from Health" $ do
        health (damage (Damage 10) create) `shouldBe` Health 990
      it "character dies when Health is 0" $ do
        isAlive (damage (Damage 1000) create) `shouldBe` False
