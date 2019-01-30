-- #1 Show the isomorphism between Maybe a and Either () a.

maybeToEither :: Maybe a -> Either () a
maybeToEither m = case m of
    Just a -> Right a
    Nothing -> Left ()

eitherToMaybe :: Either () a -> Maybe a
eitherToMaybe e = case e of
    Left _ -> Nothing
    Right a -> Just a

-- #2, 3, 4 Some Java or C++ equivalent to Haskel...

-- #5 Show that a + a = 2 × a holds for types (up to isomorphism). Re-
-- member that 2 corresponds to Bool , according to our translation
-- table.

-- a + a = 2 x a
-- a + a = 1 + 1 x a   => Bool
-- a + a = a x (1 + 1) => Distributivity (isomorphism)
-- a + a = (a x 1) + (a x 1)
-- a + a = a + a

-- Either a a = (Bool, a)

type ViaBool a = (Bool, a)

eitherToViaBool :: Either a a -> ViaBool a
eitherToViaBool e = case e of
   Left a -> (False, a)
   Right a -> (True, a) 

viaBoolToEither :: ViaBool a -> Either a a
viaBoolToEither vb = case vb of
    (False, a) -> Left a
    (True, a) -> Right a
