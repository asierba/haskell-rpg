{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Character ( create
                 , health
                 , level
                 , isAlive
                 , damage
                 , heal
                 , Health(..)
                 , Level(..)
                 , Damage(..)
                 ) where

data Character = Character { health :: Health, level :: Level, isAlive :: Bool }

newtype Health = Health Int deriving (Show, Eq, Ord, Num)
newtype Level = Level Int deriving (Show, Eq)
newtype Damage = Damage Int deriving (Show, Eq, Num)

create :: Character
create = Character { health= Health 1000, level=Level 1, isAlive=True }

damage ::  Damage -> Character -> Character
damage d c@Character{health=h} = c{health=newHealth, isAlive=newAlive}
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


