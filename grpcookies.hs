-- https://stackoverflow.com/a/38035291

import Prelude
import Data.List

-- Let's create some chips
data Chip = CHOCOLATE | MACADAMIA | CRANBERRY deriving (Ord, Eq, Show)
-- Eq allows us to do this MACADAMIA == MACADAMIA (will be true) MACADAMIA == CHOCOLATE (will be false)

-- Now cookies
data Cookie = Cookie Chip deriving Show

-- bake some cookies

cc1 = Cookie CHOCOLATE
cc2 = Cookie CHOCOLATE
cm1 = Cookie MACADAMIA
cm2 = Cookie MACADAMIA
cm3 = Cookie MACADAMIA
cr1 = Cookie CRANBERRY
cr2 = Cookie CRANBERRY
cr3 = Cookie CRANBERRY
cookies = [cc1, cm1, cr1, cc2, cc2, cm2, cm3, cr2, cr3]

-- Now, you want to compare cookies based on chips so as to categorize them! and obviously reuse our existing function to compare chips

cookie2chip (Cookie chip) = chip

{-
instance Eq Cookie where
  (==) a b  = cookie2chip(a) == cookie2chip(b)

instance Ord Cookie where
   compare a b =  compare (cookie2chip a) (cookie2chip b)
-}

-- Instead of creating Eq and Ord, you could get away just with Contravariant !

class Contravariant m where
  contramap :: (b->a) -> (m a -> m b)
-- or install and import Data.Functor.Contravariant

-- There is one complication, though  ... contramap takes only one functor (m a) as input only so need a helper like 'on' to be able to take two of them (to compare 2 money instances)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(.*.) `on` f = \x y -> f x .*. f y

-- Create a functor/container for holding an existing compare function like compare::Int->Int->Ordering above

newtype OrderFunctor a = OrderFunctor { getOrd :: a -> a -> Ordering }

instance Contravariant OrderFunctor where
  contramap f (OrderFunctor g) = OrderFunctor $ on g f

-- convert compare::Chip->Chip->OrdEnum to compare::Cookie ->Cookie->OrdEnum

instance Ord Cookie where
  compare  = getOrd . contramap cookie2chip . OrderFunctor $ compare

newtype EqFunctor a = EqFunctor { getEq :: a -> a -> Bool }

instance Contravariant EqFunctor where
  contramap f (EqFunctor g) = EqFunctor $ on g f

-- convert ==::Chip->Chip->Bool to ==::Cookie->Cookie->Bool
instance Eq Cookie where
  (==) = getEq . contramap cookie2chip . EqFunctor $ (==)

main = do
  print $ group (sort cookies)
