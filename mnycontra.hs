-- http://igstan.ro/posts/2013-10-31-contravariant-functors-an-intuition.html
import Prelude hiding(Ordering, compare, GT, LT, EQ)

-- Let's say you know how to compare things of type 'a'

data OrdEnum = GT | LT | EQ deriving Show

class Ordering a where
  compare :: a -> a -> OrdEnum
  --by allows you transform something of type like b not directly comparable, to that is comparable via f and then performs comparison
  by :: (b->a) -> b -> b -> OrdEnum
  by f x y = compare (f x) (f y)

-- Let's say we know how to compare integers

instance Ordering Int where
  compare x y = if x > y then GT else (if x < y then LT else EQ)

-- Now let's say you have Money (that stores integer value)

data Money = Money Int deriving Show

mny2int (Money a) = a

-- mint some money
m10 = Money 10
m20 = Money 20

-- Now you can reuse int compare function by transforming money to integer through mny2int
instance Ordering Money where
  compare = compare `on` mny2int

testCompareMoneyViaOrderingMoney =  do
  print $ compare m20 m10
  print $ compare m10 m10
  print $ compare m10 m20

-- Or you could use 'by' directly!
testCompareMoneyViaBy = do
  print $ by mny2int m20 m10
  print $ by mny2int m10 m10
  print $ by mny2int m10 m20

-- Or using contramap by transforming compare :: Int -> Int ->OrdEnum into compare :: Money -> Money -> OrdEnum

class Contravariant m where
  contramap :: (b->a) -> (m a -> m b)

-- There is one complication, though  ... contramap takes only one functor (m a) as input only so need a helper like 'on' to be able to take two of them (to compare 2 money instances)

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(.*.) `on` f = \x y -> f x .*. f y

mnyCompare = compare `on` mny2int

-- first let's verify that 'on' works
testOn = do
  print $ mnyCompare m20 m10
  print $ mnyCompare m10 m10
  print $ mnyCompare m10 m20

-- Create a functor/container for holding an existing compare function like compare::Int->Int->OrdEnum above

newtype OrderFunctor a = OrderFunctor { getKnown :: a -> a -> OrdEnum }

instance Contravariant OrderFunctor where
  contramap f (OrderFunctor g) = OrderFunctor $ on g f

-- convert compare::Int->Int->OrdEnum to compare::Money ->Money->OrdEnum
mnyContraCompare = getKnown . contramap mny2int . OrderFunctor $ compare

-- test compare by contramap
testContra = do
  print $ mnyContraCompare m20 m10
  print $ mnyContraCompare m10 m10
  print $ mnyContraCompare m10 m20

main = do
  putStrLn "-----Using 'by'"
  testCompareMoneyViaBy
  putStrLn "-----Using 'Ordering'"
  testCompareMoneyViaOrderingMoney
  putStrLn "-----testing 'on'"
  testOn
  putStrLn "-----Using Contramap"
  testContra
