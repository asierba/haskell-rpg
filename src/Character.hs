{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Character ( create
                 , health
                 , level
                 , isAlive
                 , damage
                 , Health(..)
                 , Level(..)
                 , Damage(..)
                 ) where

data Character = Character { health :: Health, level :: Level, isAlive :: Bool }

newtype Health = Health Int deriving (Show, Eq, Ord, Num)
newtype Level = Level Int deriving (Show, Eq)
newtype Damage = Damage Int deriving (Show, Eq)

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

