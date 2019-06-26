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
create n = Character { name = n
                     , health = Health 1000
                     , level=Level 1
                     , isAlive=True
                     }

damage :: Character -> Damage -> Character -> Character
damage from d to@Character{health=h} =
  if isSame from to
    then to
    else to{health=newHealth, isAlive=newAlive}
  where newHealth = subtractDamageHealth d h
        newAlive = newHealth > 0

isSame :: Character -> Character -> Bool
isSame c1 c2 = name c1 == name c2

subtractDamageHealth :: Damage -> Health -> Health
subtractDamageHealth (Damage d) (Health h)
  | newHealth < 0   = Health 0
  | otherwise = Health newHealth
  where newHealth = h-d

heal :: Character -> Health -> Character -> Character
heal _ _  to@Character{isAlive=False}  = to
heal from amount to =
  if charactersAreSame
    then updateHealth (improveHealth amount) to
    else to
  where charactersAreSame = isSame from to

updateHealth :: (Health -> Health) -> Character -> Character
updateHealth fn c@Character{health=h} = c { health = fn h }

improveHealth :: Health -> Health -> Health
improveHealth amount oldHealth = min 1000 (amount + oldHealth)
