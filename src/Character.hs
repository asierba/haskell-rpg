{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Character
  ( create
  , name
  , health
  , level
  , isAlive
  , damage
  , heal
  , Name(..)
  , Health(..)
  , Level(..)
  , Damage(..)
  ) where

import Data.Text

data Character =
  Character { name :: Name
            , health :: Health
            , level :: Level
            , isAlive :: Bool
            }
  deriving (Eq, Show)

newtype Name = Name Text deriving (Show, Eq)
newtype Health = Health Int deriving (Show, Eq, Ord, Num)
newtype Level = Level Int deriving (Show, Eq)
newtype Damage = Damage Int deriving (Show, Eq, Num)

create :: Name -> Character
create n = Character { name = n, health = Health 1000, level=Level 1, isAlive=True }

damage :: Character -> Damage -> Character -> Character
damage from d c@Character{health=h} =
  if name from /= name c
    then c{health=newHealth, isAlive=newAlive}
    else c
  where newHealth = subtractDamageHealth d h
        newAlive = newHealth > 0

subtractDamageHealth :: Damage -> Health -> Health
subtractDamageHealth (Damage d) (Health h)
  | newHealth < 0   = Health 0
  | otherwise = Health newHealth
  where newHealth = h-d

heal :: Health -> Character -> Character
heal _  c@Character{isAlive=False}  = c
heal h1 c@Character{health=h}
  | newHealth > 1000 = c{health=1000}
  | otherwise = c{health=newHealth}
  where newHealth = h + h1


