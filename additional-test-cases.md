# Additional Test Cases

## Pipe

1. Connecting `elements` to `elements` via a pipe ought to "unwrap"
   two levels of arrays.

   ```haskell
   > pipe elements elements (Array [Array [Number 1,Number 2], Array [Number 3,Number 4]])
   [Number 1,Number 2,Number 3,Number 4]
   ```

2. Connecting `elements` to `field "a"` via a pipe takes everything
   from an array, and then all the `"a"` fields.

   ```haskell
   > pipe elements (field "a") (Array [Object [("a",Number 1)], Object [("a", Number 2)], Object []])
   [Number 1, Number 2]
   ```

3. Connecting `field "a"` to elements via a pipe will look up the
   field `"a"` in an object and then get all the elements from the
   array stored in that field.

   ```haskell
   > pipe (field "a") elements (Object [("a", Array [Number 1, String "abc", Null])])
   [Number 1, String "abc", Null]
   ```

## Equal

1. Comparing constants that are equal:

   ```haskell
   > equal (int 1) (int 1) Null
   [Boolean True]
   ```

   `equal (int 1) (int 1)` should always give the same answer even if
   the input is not `Null`.

2. Comparing constants that are not equal:

   ```haskell
   > equal (int 1) (int 2) Null
   [Boolean False]
   ```

   `equal (int 1) (int 2)` should always give the same answer even if
   the input is not `Null`.

3. Comparing the elements of an array to a fixed integer:

   ```haskell
   > equal elements (int 1) (Array [Number 1, Number 2, Number 3, Number 4])
   [Boolean True,Boolean False,Boolean False,Boolean False]
   ```

   because `1` is equal to `1` (the first `True`), but not equal to
   `2`, `3`, or `4`.

4. Comparing the elements of an array to itself compares all possible
   pairs:

   ```haskell
   > equal elements elements (Array [Number 1, Boolean True])
   [Boolean True,Boolean False,Boolean False,Boolean True]
   ```

   Spelled out, we are comparing all pairs:

   1. `Number 1` with `Number 1`, which is true
   2. `Number 1` with `Boolean True`, which is false
   3. `Boolean True` with `Number 1`, which is false
   4. `Boolean True` with `Boolean True`, which is true

5. Comparing an element of an object with a fixed string:

   ```haskell
   > equal (field "a") (string "X") (Object [("a", String "X")])
   [Boolean True]
   ```

6. Same again, but this time the field has a value which isn't `"X"`:

   ```haskell
   > equal (field "a") (string "X") (Object [("a", String "Y")])
   [Boolean False]
   ```

## Select

1. If the condition is always true, then you get back the input:

   ```haskell
   > select (equal (int 1) (int 1)) (Array [Number 1, Number 2])
   [Array [Number 1, Number 2]]
   ```

2. If the condition is never true, then you get back the empty list:

   ```haskell
   > select (equal (int 1) (int 2)) (Array [Number 1, Number 2])
   []
   ```

3. Selecting for the `"a"` field being `1`, when it is:

   ```haskell
   > select (equal (field "a") (int 1)) (Object [("a",Number 1)])
   [Object [("a",Number 1)]]
   ```

4. Selecting for the `"a"` field being `1`, when it isn't:

   ```haskell
   > select (equal (field "a") (int 1)) (Object [("a",Number 2)])
   []
   ```

5. If the `equal` returns multiple values, then only one of them needs
   to be `True` for it to select that thing, so we can check to see if
   a certain element is in an array:

   ```haskell
   ghci> select (equal elements (int 1)) (Array [Number 1, Number 3, Number 4])
   [Array [Number 1,Number 3,Number 4]]
   ```

6. Same test, but this time with an array that doesn't contain `1`:

   ```haskell
   > select (equal elements (int 1)) (Array [Number 3, Number 4])
   []
   ```

7. Putting together `pipe`, `equal` and `select`:

   ```haskell
   > select (equal (pipe (field "a") elements) (int 1)) (Object [("a", Array [Number 1, String "abc", Null])])
   [Object [("a",Array [Number 1,String "abc",Null])]]
   ```

   and

   ```haskell
   > select (equal (pipe (field "a") elements) (int 1)) (Object [("a", Array [Number 2, String "abc", Null])])
   []
   ```
