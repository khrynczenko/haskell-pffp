-- Exercises: Will they work?

-- max (length [1, 2, 3]) (length [9, 9, 10, 11, 12])
-- This will work because length returns Int and there is instance of Ord
-- for Int

-- compare (3 * 4) (3 * 5)
-- Num a requried by `*` will default to Integer/Int which implements Ord
-- so this will work

-- compare "Julie" True
-- comaper type is a -> a -> Ordering. So type of both arguments is bound
-- to be the same. This will thus not compile.

-- (5 + 3) > (3 + 6)
-- Again will work for the same reasons second exampe works.
