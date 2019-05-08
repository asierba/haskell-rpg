module CharacterSpec where

import Test.Hspec
import Character
import Character (Health(..), Level(..), Damage(..))

spec :: Spec
spec = do
  describe "Character" $ do
    let deadCharacter = damage (Damage 1000) create
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
        isAlive deadCharacter `shouldBe` False
      it "character dies when Health is 0" $ do
        health (damage (Damage 1001) create) `shouldBe` Health 0
    describe "heal" $ do
      it "is add to Health" $ do
        let character = (damage (Damage 10) create)
        health (heal (Health 5) character) `shouldBe` Health 995
      it "dead character cannot be healed" $ do
        health (heal (Health 5) deadCharacter) `shouldBe` Health 0
        isAlive (heal (Health 5) deadCharacter) `shouldBe` False
      it "is add to Health" $ do
        let character = (damage (Damage 10) create)
        health (heal (Health 20) character) `shouldBe` Health 1000

