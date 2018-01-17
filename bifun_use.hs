-- https://www.schoolofhaskell.com/user/commercial/content/covariance-contravariance
{-# LANGUAGE RankNTypes #-}

-- consider a data structure (a functor, really)
newtype MakeString a = MakeString { toString :: a -> String }

showInt :: MakeString Int
showInt = MakeString show

show5 = toString showInt 5

-- suppose we want to add 3 and convert result into string
-- you could do this
plus3show :: MakeString Int
plus3show  = MakeString (show . (+ 3))

{-

But above is non-compositional, you are creating a new instance of MakeString 
instead of reusing one already created. What we wanted is apply one more 
function to MakeString instance, showInt.

We want to compose a MakeString instance (i.e., function stored in it) with 
another function, say f. What should the type of f be ?

Say we have MakeString g and composing g with f as (g . f).

We know:
g :: a -> String
So, f : x -> a.
What is x?

The type of resultant function stored in output has to be "b->String".
So, f has to take "b" as input.
Hence f : b -> a.

--}

mapMakeString :: forall a b. (b->a) -> MakeString a -> MakeString b
mapMakeString f (MakeString g) = MakeString ( g . f )

-- notice the type of f is  "b->a" as g expects a.
-- also, notice that "b" of this "b->a" is the same as resultant b in MakeString b
-- as composed result must return MakeString b.
--
plus3show' :: MakeString Int
plus3show' = mapMakeString (+ 3) showInt

-- nice: but, Functor is the standard pattern for for mapping inside a data structure

{-
instance Functor MakeString where
  fmap f (MakeString g) = MakeString (g . f)

There is a problem, though : fmap defines its type as:

fmap :: forall a b. (a->b) -> (MakeString a -> MakeString b)

(This is because, fmap lifts "a -> b" to "M a -> M b".)

And we know, that with 'MakeString g', g :: a -> String

So, output type of f (b) != input type of g (a)

So, we need to define a new concept:
   covariance = original and lifted functions point in the same direction
                ( a -> b and M a -> M b)
   contravariance = original and lifted functions point in opposite direction
                 (a -> b and M b -> M a)
-}

class Contravariant f where
  contramap :: (a -> b) -> f b -> f a

instance Contravariant MakeString where
  contramap f (MakeString g) = MakeString (g . f)

plus3show'' :: MakeString Int
plus3show'' = contramap (+ 3) showInt

main :: IO ()
main = do
  print $ show5
  print $ toString plus3show 5
  print $ toString plus3show' 5
  print $ toString plus3show'' 5

