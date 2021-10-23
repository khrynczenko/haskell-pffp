
foldl' _ acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs

foldr' _ acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)

-- 1.
-- foldr (*) 1 [1..5]
-- Will return the same result as which of the following?
-- a) flip (*) [1..5]
-- b) foldl (flip (*)) 1 [1..5]
-- c) foldl (*) [1..5]
-- Answer: b and c (multiplication is associative)
--
-- 2. Write down the evaluation steps for
-- foldl (flip (*)) 1 [1..3]
-- ((*) ((*) ((*) 1 1) 2) 3)

-- 3. One difference between foldr and foldl is:
-- a) foldr but not foldl, traverses the spine of a list from right to left
-- wrong: both traverse the list in the same way
-- b) foldr, but not foldl, always forces the rest of the fold (this depends on
-- data structure and the operator)
-- c) foldr, but not foldl, associates to the right
-- d) foldr, but not foldl, is rcursive (both are recursive)
-- Answer: c) foldl associates to the left

-- 4. Folds are catamporphisms, thich means they are generally used to:
-- a) Reduce structre
-- b) c) d) (pure nonsense I don't even bother writing it)
--


-- 5. Fix following folds
-- a) foldr (++) ["woot", "WOOT", "woot"]
a =  foldr (++) "" ["woot", "WOOT", "woot"]
-- b) foldr max [] "fear is the little deat"
b = foldr max (minBound :: Char) "fear is the little death"
-- c) foldr and True [False, True]
c = foldr (&&) True [False, True]
-- d) foldr (||) True [False, True]
-- There is not problem heare but it does't make sens to fold like that (will
-- return always True), accumulator should be False at the beginning
d = foldr (||) False [False, True] --
-- e) foldl ((++) . show) "" [1..5]
e = foldr ((++) . show) "" [1..5]
-- f) foldr const 'a' [1..5]
f = foldr (flip const) 'a' [1..5]
-- g) foldr const 0 "tacos"
g = foldl const 0 "tacos"
-- h) foldl (flip const) 0 "burries"
h = foldl const 0 "burries"
-- i) foldl (flip const) 'z' [1..5]
i = foldl const 'z' [1..5]



-- Why foldl unconditionally evaluates the spine while foldr does not
-- Answer: because foldl must traverse the spine independently of what is the
-- folding operation due to the implementation, foldr might not traverse 
-- the whole spine if the folding operation does not evaluate the second
-- argument.
