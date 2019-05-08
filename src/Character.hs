module Character ( create
                 , health
                 , level
                 , isAlive
                 , Health(..)
                 , Level(..)
                 ) where

data Character = Character { health :: Health, level :: Level, isAlive :: Bool }

newtype Health = Health Int deriving (Show, Eq)
newtype Level = Level Int deriving (Show, Eq)

create :: Character
create = Character { health= Health 1000, level=Level 1, isAlive=True }

