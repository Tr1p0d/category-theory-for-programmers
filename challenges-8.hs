import Control.Monad ((>=>))
import Control.Monad.Identity (Identity(Identity))
import Data.Bifunctor
import Data.Functor.Const (Const(Const))

fmap' f = id >=> \x -> return (f x)

-- #1 Show that the data type:

data Pair a b = Pair a b

-- is a bifunctor. For additional credit implement all three methods
-- of Bifunctor and use equational reasoning to show that these def-
-- initions are compatible with the default implementations when-
-- ever they can be applied.

-- 1.a) Pair is isomorphic to (.) for which the proof exists

pairToTuple :: Pair a b -> (a, b)
pairToTuple (Pair a b) = (a, b)

tupleToPair :: (a, b) -> Pair a b
tupleToPair (a, b) = Pair a b

-- 1.b) Bifunctor instance

-- The 'For additional credit' really means that we should proove that
-- Pair is a CommutativeBifunctor akka: 
-- bimap fa fb = first fa . second fb = second fb . first fa

instance Bifunctor Pair where
  bimap fa fb (Pair a b) = Pair (fa a) (fb b)
  first fa (Pair a b) = Pair (fa a) b 
  second fb (Pair a b) = Pair a (fb b)

-- #2 Show the isomorphism between the standard definition of Maybe
-- and this desugaring:

type Maybe' a = Either (Const () a) (Identity a)

-- For additional credit, show that they are the inverse of each other
-- using equational reasoning.

maybe'ToMaybe :: Maybe' a -> Maybe a
maybe'ToMaybe (Left _) = Nothing
maybe'ToMaybe (Right (Identity a)) = Just a

maybeToMaybe' :: Maybe a -> Maybe' a
maybeToMaybe' Nothing = Left (Const ())
maybeToMaybe' (Just a) = Right (Identity a)

-- 3# Let’s try another data structure. I call it a PreList because it’s a
-- precursor to a List . It replaces recursion with a type parameter b .

data PreList a b = Nil | Cons a b

-- You could recover our earlier definition of a List by recursively
-- applying PreList to itself (we’ll see how it’s done when we talk
-- about fixed points).
-- Show that PreList is an instance of Bifunctor .

newtype Fix f = Fix (f (Fix f))

type List a = Fix (PreList a)

instance Bifunctor PreList where
    bimap _ _ Nil = Nil
    bimap fa fb (Cons a b) = Cons (fa a) (fb b)

    first _ Nil = Nil
    first fa (Cons a b) = Cons (fa a) b

    second _ Nil = Nil
    second fb (Cons a b) = Cons a (fb b)

-- #4 Show that the following data types define bifunctors in a and b :

data K2 c a b = K2 c
data Fst a b = Fst a
data Snd a b = Snd b

instance Bifunctor (K2 c) where
    bimap _ _ (K2 c) = K2 c

instance Bifunctor Fst where
    bimap fa _ (Fst a) = Fst (fa a)

instance Bifunctor Snd where
    bimap _ fb (Snd b) = Snd (fb b)
