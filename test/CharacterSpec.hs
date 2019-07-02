{-# LANGUAGE OverloadedStrings #-}

module CharacterSpec where

import Test.Hspec
import Character
import Character (Health(..), Level(..), Damage(..))

spec :: Spec
spec = do
  describe "Character" $ do
    let character1 = create $ Name "Player 1"
    let character2 = create $ Name "Player 2"
    let deadCharacter = damage character1 (Damage 1000) (create $ Name "Dead Player")
    describe "create" $ do
      it "creates a character with its given name" $ do
        name character1 `shouldBe` Name "Player 1"
      it "creates a character with health at 1000" $ do
        health character1 `shouldBe` Health 1000
      it "creates a character with a level 1" $ do
        level character1 `shouldBe` Level 1
      it "creates a character alive" $ do
        isAlive character1 `shouldBe` True

    describe "levelUp" $ do
      it "increases the character's level by 1" $ do
        level (levelUp character1) `shouldBe` Level 2
        level (levelUp $ levelUp character1) `shouldBe` Level 3

    describe "damage" $ do
      it "is subtracted from Health" $ do
        health (damage character2 (Damage 10) character1) `shouldBe` Health 990
      it "character dies when Health is 0" $ do
        isAlive deadCharacter `shouldBe` False
      it "character dies when Health is 0" $ do
        health (damage character2 (Damage 1001) character1) `shouldBe` Health 0
      it "cannot damage itself" $ do
        damage character1 (Damage 10) character1 `shouldBe` character1
      it "cannot damage a player with the same name" $ do
        let otherPlayer1 = damage character2 (Damage 5) (create $ Name "Player 1")
        damage otherPlayer1 (Damage 10) character1 `shouldBe` character1
      it "increases the damage by 50% if the attacker is 5 or more levels higher" $ do
        let strongCharacter = applyN 5 levelUp (create $ Name "Strong")
        health (damage strongCharacter (Damage 10) character1) `shouldBe` Health 985

    describe "heal" $ do
      let damagedCharacter = damage character2 (Damage 10) (create $ Name "Damaged")
      it "is add to Health" $ do
        health (heal damagedCharacter (Health 5) damagedCharacter) `shouldBe` Health 995
      it "dead character cannot be healed" $ do
        health (heal deadCharacter (Health 5) deadCharacter) `shouldBe` Health 0
        isAlive (heal deadCharacter (Health 5) deadCharacter) `shouldBe` False
      it "is add to Health" $ do
        health (heal damagedCharacter (Health 20) damagedCharacter) `shouldBe` Health 1000
      it "cannot health other characters" $ do
        health (heal character1 (Health 20) damagedCharacter) `shouldBe` Health 990
      it "can healh characters with the same name" $ do
        health (heal (create $ Name "Damaged") (Health 20) damagedCharacter) `shouldBe` Health 1000

applyN :: Int -> (a -> a) -> a -> a
applyN n f = foldr (.) id (replicate n f)
