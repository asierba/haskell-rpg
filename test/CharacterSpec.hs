{-# LANGUAGE OverloadedStrings #-}

module CharacterSpec where

import Test.Hspec
import Character
import Character (Health(..), Level(..), Damage(..))

spec :: Spec
spec = do
  describe "Character" $ do
    let player1 = create $ Name "Player 1"
    let player2 = create $ Name "Player 2"
    let deadCharacter = damage player1 (Damage 1000) (create $ Name "Dead Player")
    describe "create" $ do
      it "creates a character with its given name" $ do
        name player1 `shouldBe` Name "Player 1"
      it "creates a character with health at 1000" $ do
        health player1 `shouldBe` Health 1000
      it "creates a character with a level 1" $ do
        level player1 `shouldBe` Level 1
      it "creates a character alive" $ do
        isAlive player1 `shouldBe` True
    describe "damage" $ do
      it "is subtracted from Health" $ do
        health (damage player2 (Damage 10) player1) `shouldBe` Health 990
      it "character dies when Health is 0" $ do
        isAlive deadCharacter `shouldBe` False
      it "character dies when Health is 0" $ do
        health (damage player2 (Damage 1001) player1) `shouldBe` Health 0
      it "cannot damage itself" $ do
        damage player1 (Damage 10) player1 `shouldBe` player1
      it "cannot damage a player with the same name" $ do
        let otherPlayer1 = damage player2 (Damage 5) (create $ Name "Player 1")
        damage otherPlayer1 (Damage 10) player1 `shouldBe` player1
    describe "heal" $ do
      it "is add to Health" $ do
        let character = (damage player2 (Damage 10) player1)
        health (heal (Health 5) character) `shouldBe` Health 995
      it "dead character cannot be healed" $ do
        health (heal (Health 5) deadCharacter) `shouldBe` Health 0
        isAlive (heal (Health 5) deadCharacter) `shouldBe` False
      it "is add to Health" $ do
        let character = (damage player2 (Damage 10) player1)
        health (heal (Health 20) character) `shouldBe` Health 1000

