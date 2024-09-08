{-# LANGUAGE GHC2021 #-}

import Data.Subtypes

import GHC.Generics (Generic)

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

dog4 :: FourLeggedAnimal
dog4 = Dog4 (MkDog "Snowy" 4)

data Lang
    = Lit Int
    | Unit
    deriving (Generic, Show)

main :: IO ()
main =
    do
        print (inj dog4 :: Animal)
        print (proj cat :: Maybe FourLeggedAnimal)
        print (proj duck :: Maybe FourLeggedAnimal)
        print (inj dog :: Animal)

        print (inj (MkDog "Doggo" 3) :: FourLeggedAnimal)

        print (inj (5 :: Int) :: Lang)
        print (inj () :: Lang)
