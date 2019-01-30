-- #1 Can we turn the Maybe type constructor into a functor by defining:
-- fmap _ _ = Nothing

-- fmap id = id
-- fmap (f . g) = fmap f . fmap g

-- Identity law
-- fmap id (Just a)
-- = Nothing // Whoops

-- #2 Prove functor laws for the reader functor. Hint: it’s really simple.
-- fmap = (.)
--
-- Identity and associativity holds for function composition.

-- #4 Prove the functor laws for the list functor. Assume that the laws
-- are true for the tail part of the list you’re applying it to (in other
-- words, use induction). 

-- 1) Identity fmap id = id

-- 1.a) Nil identity 
-- fmap id Nil 
-- = Nil => fmap
-- = id Nil => identity

-- 1.b) Cons identity
-- fmap id (Cons a as)
-- = Cons (id a) (fmap id as) => induction
-- = Cons (id a) as => id
-- = Cons a as => id
-- = id (Cons a as)

-- 2) Associativity fmap (f . g) = fmap f , fmap g

-- 2.a) Nil associativity
-- (fmap f . fmap g) Nil
-- = fmap f ( fmap g Nil ) => .
-- = fmap f ( Nil ) => fmap
-- = Nil => fmap
-- = fmap (f . g) Nil => fmap

-- 2.b) Cons associativity
-- (fmap f . fmap g) (Cons a as)
-- fmap f ( fmap g (Cons a as)) => .
-- fmap f ( Cons (g a) (fmap g as)) => fmap
-- fmap f ( Cons (g a) (fmap g as)) => fmap
-- Cons (f (g a)) (fmap f (fmap g as)) => fmap
-- Cons ((f . g) a) ((fmap f . fmap g) as) => .
-- Cons ((f . g) a) (fmap (f . g) as) => induction
-- fmap (f . g) (Cons a as) => fmap
