{-# LANGUAGE GHC2021 #-}

import Data.Subtypes.Parametric

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

dog4 :: FourLeggedAnimal
dog4 = Dog4 (MkDog "Snowy" 4)

injGen :: (Generic sup, Rec0 sub :<: Rep sup) => sub -> sup
injGen = to . inj . K1 @_ @R

injGenB :: (Generic sub, Generic sup, Rep sub :<: Rep sup) => sub -> sup
injGenB = to . inj . from

projGenB :: (Generic sub, Generic sup, Rep sub :<: Rep sup) => sup -> Maybe sub
projGenB = fmap to . proj . from

main :: IO ()
main =
    do
        print (injGenB dog4 :: Animal)
        print (projGenB cat :: Maybe FourLeggedAnimal)
        print (projGenB duck :: Maybe FourLeggedAnimal)
        print (injGenB dog :: Animal)

        print (injGen (MkDog "Doggo" 3) :: FourLeggedAnimal)
