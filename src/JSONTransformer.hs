module JSONTransformer (Transformer, field, select, pipe, string, int, comparison, elements, bool, tNull, concatenate, identity, tAnd, tOr, tNot, tAdd, tSub, tMul, tDiv, tMod) where

import JSON
import Result

-- | A 'Transformer' is a function that takes a single 'JSON' value
-- and returns a list of 'JSON' values.
type Transformer = JSON -> Result [JSON]

-- HINT: the design of these transformers is based on the design of
-- the Jq tool, and this paper on querying XML:
--
--   "Haskell and XML: generic combinators or type-based translation?"
--   by Malcolm Wallace and Colin Runciman
--   https://dl.acm.org/doi/10.1145/317765.317794
--
-- Section 2 of the paper covers what we're doing. The 'CFilter' type
-- is what we called 'Transformer' here.


-- | Ignores the 'JSON' input and returns the given string as a piece
-- of 'JSON' in a one element list.
--
-- For example,
--
--  > string "hello" (Number 1)
--
-- gives
--
--  > [String "hello"]
string :: String -> Transformer
string s _ = Ok [String s]

-- | Ignores the 'JSON' input and returns the given integer as a piece
-- of 'JSON' in a one element list.
--
-- For example,
--
--  > int 1234 (Number 1)
--
-- gives
--
--  > [Number 1234]
int :: Int -> Transformer
int i _ = Ok [Number i]

-- HINT: these two functions are similar to the 'literal' function in
-- the paper linked above.


-- | Returns all the elements of a `JSON` `Array`. If the `JSON` value
-- is not an `Array`, then it returns an error.
--
-- For example,
--
--  > elements (Array [Number 1, Number 2, Boolean True])
--
-- returns
--
--  > [Number 1, Number 2, Boolean True]
--
-- and
--
--  > elements (Number 1)
--
-- returns
--
--  > []
--
-- because 'Number 1' is not an array.
elements :: Transformer
elements arr = case getElements arr of
    Nothing -> Error "Not an array"
    Just xs -> Ok xs

-- HINT: you can use the 'getElements' function from the 'JSON'
-- module.

-- | Looks up a field in a 'JSON' 'Object' and returns the value
-- associated with that field in a list by itself. If the 'JSON' value
-- is not an object, or the field does not exist, then returns the
-- empty list.
--
-- For example,
--
--  > field "a" (Object [("a", Number 1)])
--
-- returns
--
--  > [Number 1]
--
-- and
--
--  > field "b" (Object [("a", Number 1)])
--
-- returns
--
--  > []
--
-- because the field "b" is not in the object.
field :: String -> Transformer
field k json = case getField k json of
    Nothing -> Error ("Field '" ++ k ++ "' not found / Not an object")
    Just x  -> Ok [x]

-- HINT: use 'getField' from the 'JSON' module to define this
-- function.

-- | Connects two transformers together, feeding the output of the
-- first into the input of the second, and then flattening all the
-- results.
--
-- A picture, where 'x' is the input, 'f' is the first transformer,
-- and 'g' is the second.
--
-- @@
--            [v1,   --g--> [[x1,         [x1,
--                            x2],         x2,
-- x  --f-->   v2,   --g-->  [x3,    -->   x3,
--                            x4],         x4,
--             v3]   --g-->  [x5,          x5,
--                            x6]]         x6]
-- @@
pipe :: Transformer -> Transformer -> Transformer
pipe t1 t2 json =
    do xs <- t1 json
       ys <- mapM t2 xs
       return (concat ys)

-- HINT: this function is the 'o' function in the paper linked above.

-- | Takes a comparison function, two transformers and an input.
-- Applies the input to the two transformers to get two lists of
-- values. Compares all pairs of these values, returning 
-- 'Boolean True' or 'Boolean False' for each one.
--
-- For example,
--
--   > comparison (==) (string "a") (string "a") (Number 1)
--
-- gives
--
--  > [Boolean True]
--
-- and
--
--  > comparison (==) (string "a") (string "b") (Number 1)
--
-- gives
--
--  > [Boolean False]
--
-- This function was previously named 'equal', but has been
-- redefined to be more general.
comparison :: (JSON -> JSON -> Bool) -> Transformer -> Transformer -> Transformer
comparison c t1 t2 json =
    do xs <- t1 json
       ys <- t2 json
       return [ Boolean (x `c` y) | x <- xs, y <- ys ]

-- HINT: the easiest way to write this function is to use a list
-- comprehension (Week 4) to get all the pairs returned by the two
-- transformers.

-- | Filter the input. If the transformer argument returns 'true' for
-- the input, then return the input in a single element list. If the
-- transformer argument does not return 'true' then return '[]'.
--
-- Note: this is the function as originally described, but its use is
-- replaced by 'select' below.
select0 :: Transformer -> Transformer
select0 t json =
    do js <- t json
       if any (extractMaybeBool . getBool) js then return [json] else return []

extractMaybeBool :: Maybe Bool -> Bool
extractMaybeBool (Just True) = True
extractMaybeBool _           = False

-- HINT: you'll need to check to see if the transformer argument
-- returns 'true' at any point in its list. You can use the 'any'
-- function (Week 05) to do this. You'll also need to extract a
-- boolean from the 'JSON' value using 'getBool' in the 'JSON'
-- module. You might want to write a helper function to convert a
-- 'Maybe Bool' to a 'Bool'.

-- | Additional Transformers | --

-- | Filter the input. If the transformer argument returns truthy for
-- the input, then return the input in a single element list. If the
-- transformer argument does not return truthy then return '[]'.
select :: Transformer -> Transformer
select t json =
    do js <- t json
       if any getTruthy js then return [json] else return []

-- | Ignores the 'JSON' input and returns the given boolean as a piece
-- of 'JSON' in a one element list.
bool :: Bool -> Transformer
bool b _ = Ok [Boolean b]

-- | Ignores the 'JSON' input and returns 'Null' as a piece of 'JSON'
-- in a one element list.
tNull :: Transformer
tNull _ = Ok [Null]

-- | Concatenates the results of two transformers into a single list.
concatenate :: Transformer -> Transformer -> Transformer
concatenate t1 t2 json =
    do xs <- t1 json
       ys <- t2 json
       case ys of
           [Array as] -> return (xs ++ as)
           _          -> return (xs ++ ys)

-- | Returns the input unchanged.
identity :: Transformer
identity json = Ok [json]

-- | Returns whether both transformers evaluate to a truthy value.
tAnd :: Transformer -> Transformer -> Transformer
tAnd t1 t2 json =
    do xs <- t1 json
       ys <- t2 json
       return [Boolean (all getTruthy xs && all getTruthy ys)]

-- | Returns whether either transformer evaluates to a truthy value.
tOr :: Transformer -> Transformer -> Transformer
tOr t1 t2 json =
    do xs <- t1 json
       ys <- t2 json
       return [Boolean (any getTruthy xs || any getTruthy ys)]

-- | Returns the opposite of the transformer's truthy value.
tNot :: Transformer -> Transformer
tNot t json =
    do xs <- t json
       return [Boolean (not (any getTruthy xs))]

-- | Returns the result of an addition operation on the two transformers' results.
--
-- Operations:
--
--   Numbers : arithmetic operations
--
--   Null + Any : Return the non-null value
--
--   Arrays : concatenate two arrays
--
--   Strings : concatenate two strings
--
--   Objects : merge the two objects (Unimplemented)
tAdd :: Transformer -> Transformer -> Transformer
tAdd t1 t2 json =
    do xs <- t1 json
       ys <- t2 json
       (case (xs, ys) of
           ([Number x], [Number y]) -> return [Number (x + y)]
           ([Null],     y         ) -> return y
           (x,          [Null]    ) -> return x
           ([Array as], [Array ys]) -> return [Array (as ++ ys)]
           ([String s], [String t]) -> return [String (s ++ t)]
           -- Objects : merge the two objects (Unimplemented)
           _                        -> Error "Invalid addition operation")

-- | Returns the result of a subtraction operation on the two transformers' results.
-- 
-- Operations:
--
--   Numbers : arithmetic operations
--
--   Arrays : remove all occurrences of the second array's elements from the first array (Unimplemented)
tSub :: Transformer -> Transformer -> Transformer
tSub t1 t2 json =
    do xs <- t1 json
       ys <- t2 json
       (case (xs, ys) of
           ([Number x], [Number y]) -> return [Number (x - y)]
           -- Arrays : remove all occurrences of the second array's elements from the first array (Unimplemented)
           _                        -> Error "Invalid subtraction operation")

-- | Returns the result of a multiplication operation on the two transformers' results.
--
-- Operations:
--
--   Numbers : arithmetic operations
--
--   String * Number : repeat the string n times
tMul :: Transformer -> Transformer -> Transformer
tMul t1 t2 json =
    do xs <- t1 json
       ys <- t2 json
       (case (xs, ys) of
           ([Number x], [Number y]) -> return [Number (x * y)]
           ([Number n], [String s]) -> return [String (concat (replicate n s))]
           ([String s], [Number n]) -> return [String (concat (replicate n s))]
           _                        -> Error "Invalid multiplication operation")

-- | Returns the result of a division operation on the two transformers' results.
--
-- Operations:
--
--   Numbers : arithmetic operations
--
--   Strings : Split the first using the second as separators (Unimplemented)
tDiv :: Transformer -> Transformer -> Transformer
tDiv t1 t2 json =
    do xs <- t1 json
       ys <- t2 json
       (case (xs, ys) of
           ([Number x], [Number y]) -> return [Number (x `div` y)]
           -- Strings : Split the first using the second as separators (Unimplemented)
           _                        -> Error "Invalid division operation")

-- | Returns the result of a modulus operation on the two transformers' results.
--
-- Operations:
--
--   Numbers : arithmetic operations
tMod :: Transformer -> Transformer -> Transformer
tMod t1 t2 json =
    do xs <- t1 json
       ys <- t2 json
       (case (xs, ys) of
           ([Number x], [Number y]) -> return [Number (x `mod` y)]
           _                        -> Error "Invalid modulus operation")