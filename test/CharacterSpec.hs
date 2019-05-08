module CharacterSpec where

import Test.Hspec
import qualified Character
import Character (Health(..), Level(..))

spec :: Spec
spec = do
  describe "Character" $ do
    describe "create" $ do
      it "creates a character with health at 1000" $ do
        Character.health Character.create `shouldBe` Health 1000
      it "creates a character with a level 1" $ do
        Character.level Character.create `shouldBe` Level 1
      it "creates a character alive" $ do
        Character.isAlive Character.create `shouldBe` True
