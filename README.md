# cs316-json-parser

A Haskell JSON parser built in the style of jQuery. Created from specifications provided in the University of Strathclyde's CS316: Functional Programming class.

For the original specification and build instructions, please see [here](SPECIFICATION.md). This new README is created for the purpose of documenting features available in this implementation.

## Features

After specifying the JSON file to load from as a command line argument, you will be prompted to enter a query. Queries can use any of these filters in combination.

---

### Identity: `.`

Returns the input unchanged.

##### Example
Input: `{"a": 1, "b": 2}`
Query: `.`
Output: `{"a": 1, "b": 2}`

---

### Object Identifier-Index: `.a`, `.a.b`

Looks up a field in the object and returns the value associated with that field.

If the key contains disallowed characters, it will need to be surrounded with double quotes like this: `.".a"` or `.[".a"]`

##### Examples
Input: `{"a": 1, "b": 2}`
Query: `.a`
Output: `1`

Input: `{"a": {"b": 2}}`
Query: `.a.b`
Output: `2`

Input: `{"a": {"...": 2}}`
Query: `.a.["..."]`
Output: `2`

---

### Elements: `.[]`

Returns all the elements of an array.

##### Example
Input: `[1, 2, 3]`
Query: `.[]`
Output:
`1`
`2`
`3`

---

### Comma: `,`

Runs two filters on the input and concatenates the results in order.

##### Example
Input: `{"a": 1, "b": 2}`
Query: `.a, .b`
Output: 
`1`
`2`

---

### Pipe: `|`

Combines two filters by feeding the output of the first into the second. If the first filter produces multiple results, the second filter will be run on each of them.

##### Example
Input: `{"a": {"b": 2}}`
Query: `.a | .b`
Output: `2`

---

### Constant Values: `"string"`, `true`, `null`, `2`

Strings, Booleans, null & positive Integers are all interpreted as their constant representations.

---

### Addition: `+`

Takes two filters, applies them to the input, and adds the results together.

- Numbers are added by normal arithmetic.
- Anything added to null is unchanged.
- Arrays are concatenated together.
- Strings are concatenated together.

##### Examples
Input: `{"a": 1, "b": 2}`
Query: `.a + .b`
Output: `3`

Input: `{"a": 1, "b": 2}`
Query: `.a + null`
Output: `1`

Input: `{"a": [1, 2], "b": [3, 4]}`
Query: `.a + .b`
Output: `[1, 2, 3, 4]`

Input: `{"a": "hello", "b": " world"}`
Query: `.a + .b`
Output: `"hello world"`

---

### Subtraction

Takes two filters, applies them to the input, and subtracts the result of the second from the first.

- Numbers are subtracted by normal arithmetic.

##### Example
Input: `{"a": 3, "b": 2}`
Query: `.a - .b`
Output: `1`

---

### Multiplication

Takes two filters, applies them to the input, and multiplies the results together.

- Numbers are multiplied by normal arithmetic.
- Multiplying a string by a number will repeat the string that many times.

##### Examples
Input: `{"a": 3, "b": 2}`
Query: `.a * .b`
Output: `6`

Input: `{"a": "hi", "b": 3}`
Query: `.a * .b`
Output: `"hihihi"`

---

### Division

Takes two filters, applies them to the input, and divides the result of the first by the second.

- Numbers are divided by integer division.

##### Example
Input: `{"a": 6, "b": 2}`
Query: `.a / .b`
Output: `3`

---

### Modulo

Takes two filters, applies them to the input, and returns the remainder of the first divided by the second.

- Numbers are divided by normal arithmetic.

##### Example
Input: `{"a": 5, "b": 2}`
Query: `.a % .b`
Output: `1`

---

### `select(expression)`

Produces its input unchanged if the expression evaluates as truthy (anything except `false` or `null`), or nothing otherwise.

##### Example
Input: `{"a": 1, "b": 2}`
Query: `select(true)`
Output: `{"a": 1, "b": 2}`

---

### `==` & `!=`

Takes a filter from either side, applies them to the input, and returns true if the results are equal or not equal respectively, or false otherwise.

##### Example
Input: `{"a": 1, "b": 2}`
Query: `.a == .b`
Output: `false`

---

### `<`, `<=`, `>`, `>=`

Takes a filter from either side, applies them to the input, and returns whether the first is less than, less than or equal to, greater than, or greater than or equal to the second respectively.

##### Example
Input: `{"a": 1, "b": 2}`
Query: `.a < .b`
Output: `true`

---

### `and`, `or` [`&&`, `||`]

Takes a filter from either side, applies them to the input, and follows standard boolean logic. Anything except `False` or `null` is considered truthy & will satisfy these filters. `and` has higher precedence than `or`.

##### Example
Input: `{"a": 1, "b": 2}`
Query: `.a > 0 and .b > 0`
Output: `true`

---

### `not` [`!`]

Takes a filter, applies it to the input, and inverts the truthy value of the result.

##### Example
Input: `{"a": 1, "b": 2}`
Query: `not (.a > 0)`
Output: `false`

---

## Additional Extensions

- Queries that have not been parsed fully will be rejected with an error message.
- The `Transformer` type has been altered to `JSON -> Result [JSON]` and reports appropriate errors on failure.
- All functions have provided documentation.

## Test Suite

A test suite has been implemented for every function written for this coursework, and can be found in `./src/Spec.hs`.

## Example Queries (inexhaustive) (`hills.json`)

- `.` - Returns the entire JSON file.
- `.[]` - Returns all the hills separately.
- `.[].Name` - Returns the names of all the hills.
- `.[] | select(.Height > 1000) | .Name` - Returns the names of all the hills over 1000m.
- `.[] | select(.Height > 1000 && .Country == "S") | .Name, .Height` - Returns the names and heights of all the hills over 1000m in Scotland.
- `.[] | select(.Height > 1300 or not (.Height >= 50))` - Returns all the hills over 1300m or under 50m.
- `.[] | select(.Country != "S")` - Returns all the hills that are not in Scotland.
- `.[] | select(.Height > 1100) | .Name , (.Height | . - 1100)` - Returns the names of all the hills over 1100m, and by how much they exceed 1100m.
- `2 * 10 / 2 * (3 - 1) + 5` - Returns 25.
