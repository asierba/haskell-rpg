module Character ( create, health, level, isAlive ) where

data Character = Character { health :: Int, level :: Int, isAlive :: Bool }

create :: Character
create = Character { health=1000, level=1, isAlive=True }

