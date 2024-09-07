{-# LANGUAGE GHC2021 #-}

import Data.Comp.Ops

import GHC.Generics

-- Examples taken from Csongor Kiss's
-- generic-lens:Data.Generics.Sum.Subtype's documentation

data Animal
    = Dog Dog
    | Cat Name Age
    | Duck Age
    deriving (Generic, Show)

data FourLeggedAnimal
    = Dog4 Dog
    | Cat4 Name Age
    deriving (Generic, Show)

data Dog = MkDog
    { name :: Name
    , age  :: Age
    } deriving (Generic, Show)

type Name = String
type Age  = Int

dog, cat, duck :: Animal
dog = Dog (MkDog "Shep" 3)
cat = Cat "Mog" 5
duck = Duck 2

dog4, cat4 :: FourLeggedAnimal
dog4 = Dog4 (MkDog "Snowy" 4)
cat4 = Cat4 "Garfield" 6

main :: IO ()
main = pure ()
