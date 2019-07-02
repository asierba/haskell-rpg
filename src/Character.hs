{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

{-
## Iteration Two
A Character cannot Deal Damage to itself. [DONE]

A Character can only Heal itself. [DONE]

When dealing damage:

- If the target is 5 or more Levels above the attacker, Damage is reduced by 50% [NOT DONE - only for 5 so far]
- If the target is 5 or more levels below the attacker, Damage is increased by 50% [NOT DONE]
-}

module Character
  ( create
  , name
  , health
  , level
  , isAlive
  , damage
  , heal
  , levelUp
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
newtype Level = Level Int deriving (Show, Eq, Enum, Num)
newtype Damage = Damage Int deriving (Show, Eq, Num)

create :: Name -> Character
create n = Character { name = n
                     , health = Health 1000
                     , level=Level 1
                     , isAlive=True
                     }

levelUp :: Character -> Character
levelUp c@Character{level} = c { level = succ level}

damage :: Character -> Damage -> Character -> Character
damage from@Character{level=fromLevel} (Damage d) to@Character{level=toLevel} =
  if isSame from to
    then to
    else updateHealth (subtractDamageHealth (Damage damageAmount)) to
  where damageAmount = if fromLevel - toLevel == 5 then d + (d `div` 2) else d

updateHealth :: (Health -> Health) -> Character -> Character
updateHealth fn c@Character{health, isAlive} =
  if isAlive
     then c { health = newHealth, isAlive = newIsAlive }
     else c
  where newHealth = fn health
        newIsAlive = newHealth > 0

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

improveHealth :: Health -> Health -> Health
improveHealth amount oldHealth = min 1000 (amount + oldHealth)
