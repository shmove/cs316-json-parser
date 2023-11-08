import System.Exit
import Test.QuickCheck ()
import Test.HUnit

import JSON
import JSONOutput
import JSONTransformer

-- | JSONOutput.hs | --

-- Rendering a JSON string, escaping special characters defined at https://www.json.org/
test_render_json_string = TestCase (assertEqual ("for renderJSON (String \"\\b\\\"hello world \\o/\\n!!!\\f\\r\\t\\\"\")") "\"\\b\\\"hello world \\\\o\\/\\n!!!\\f\\r\\t\\\"\"" (renderJSON (String "\b\"hello world \\o/\n!!!\f\r\t\"")))

-- Rendering a JSON boolean (true and false)
test_render_json_bool_true = TestCase (assertEqual ("for renderJSON (Boolean True)") "true" (renderJSON (Boolean True)))
test_render_json_bool_false = TestCase (assertEqual ("for renderJSON (Boolean False)") "false" (renderJSON (Boolean False)))

-- Rendering a JSON null
test_render_json_null = TestCase (assertEqual ("for renderJSON Null") "null" (renderJSON Null))

-- Rendering a JSON number
test_render_json_num = TestCase (assertEqual ("for renderJSON (Number 4)") "4" (renderJSON (Number 4)))

-- Rendering a JSON array
test_render_json_arr = TestCase
  (assertEqual ("for renderJSON (Array [Number 1, Number 2, Number 3])")
  "[1, 2, 3]"
  (renderJSON (Array [Number 1, Number 2, Number 3])))

-- Rendering a JSON object
test_render_json_obj = TestCase (assertEqual ("for renderJSON (Object [(\"a\", Number 1), (\"b\", Number 2)])") "{\"a\": 1, \"b\": 2}" (renderJSON (Object [("a", Number 1), ("b", Number 2)])))

jsonOutputTests = TestLabel "JSONOutput Tests" (TestList
  [
    TestLabel "render_json_string" test_render_json_string,
    TestLabel "render_json_bool_true" test_render_json_bool_true,
    TestLabel "render_json_bool_false" test_render_json_bool_false,
    TestLabel "render_json_null" test_render_json_null,
    TestLabel "render_json_num" test_render_json_num,
    TestLabel "render_json_arr" test_render_json_arr,
    TestLabel "render_json_obj" test_render_json_obj
  ])



-- | JSONTransformer.hs | --

-- Ignores JSON input and returns the given string as a piece of JSON in a one element list.
test_string = TestCase (assertEqual ("for string \"hello\" (Number 1)") [String "hello"] (string "hello" (Number 1)))

-- Ignores JSON input and returns the given integer as a piece of JSON in a one element list.
test_int = TestCase (assertEqual ("for int 1234 (Number 1)") [Number 1234] (int 1234 (Number 1)))

-- Returns all the elements of a JSON Array. If the JSON value is not an Array, then it returns the empty list.
test_elements = TestCase (assertEqual ("for elements (Array [Number 1, Number 2, Boolean True])") [Number 1, Number 2, Boolean True] (elements (Array [Number 1, Number 2, Boolean True])))
test_elements_invalid = TestCase (assertEqual ("for elements (Number 1)") [] (elements (Number 1)))

-- Looks up a field in a JSON Object and returns the value associated with that field in a one element list. If the JSON value is not an Object, or the field does not exist, then returns the empty list.
test_field = TestCase (assertEqual ("for field \"a\" (Object [(\"a\", Number 1)])") [Number 1] (field "a" (Object [("a", Number 1)])))
test_field_invalid = TestCase (assertEqual ("for field \"a\" (Number 1)") [] (field "a" (Number 1)))
test_field_nonexistent = TestCase (assertEqual ("for field \"b\" (Object [(\"a\", Number 1)])") [] (field "b" (Object [("a", Number 1)])))

-- Connecting elements to elements via a pipe unwraps two levels of arrays
test_pipe_unwrap = TestCase (assertEqual ("for pipe elements elements (Array [Array [Number 1,Number 2], Array [Number 3,Number 4]])") [Number 1,Number 2,Number 3,Number 4] (pipe elements elements (Array [Array [Number 1,Number 2], Array [Number 3,Number 4]])))

-- Compares all pairs of values from the two transformed lists for equality, returning 'Boolean True' or 'Boolean False' for each one.
test_equal_truthy = TestCase (assertEqual ("for equal (string \"a\") (string \"a\") (Number 1)") [Boolean True] (equal (string "a") (string "a") (Number 1)))
test_equal_falsy = TestCase (assertEqual ("for equal (string \"a\") (string \"b\") (Number 1)") [Boolean False] (equal (string "a") (string "b") (Number 1)))

-- Filter the input. If the transformer argument returns 'true' for the input then return the input in a single element list, else return the empty list.
test_select_match_condition = TestCase (assertEqual ("for select (equal (field \"a\") (int 1)) (Object [(\"a\",Number 1)])") [Object [("a",Number 1)]] (select (equal (field "a") (int 1)) (Object [("a",Number 1)])))
test_select_nomatch_condition = TestCase (assertEqual ("for select (equal (field \"a\") (int 1)) (Object [(\"a\",Number 2)])") [] (select (equal (field "a") (int 1)) (Object [("a",Number 2)])))
test_select_anymatch = TestCase (assertEqual ("for select (equal elements (int 1)) (Array [Number 1, Number 3, Number 4])") [Array [Number 1,Number 3,Number 4]] (select (equal elements (int 1)) (Array [Number 1, Number 3, Number 4])))
test_select_nonematch = TestCase (assertEqual ("for select (equal elements (int 1)) (Array [Number 3, Number 4])") [] (select (equal elements (int 1)) (Array [Number 3, Number 4])))

jsonTransformerTests = TestLabel "JSONTransformer Tests" (TestList
  [
    TestLabel "string" test_string,
    TestLabel "int" test_int,
    TestLabel "elements" test_elements,
    TestLabel "elements_invalid" test_elements_invalid,
    TestLabel "field" test_field,
    TestLabel "field_invalid" test_field_invalid,
    TestLabel "field_nonexistent" test_field_nonexistent,
    TestLabel "pipe_unwrap" test_pipe_unwrap,
    TestLabel "equal_truthy" test_equal_truthy,
    TestLabel "equal_falsy" test_equal_falsy,
    TestLabel "select_match_condition" test_select_match_condition,
    TestLabel "select_nomatch_condition" test_select_nomatch_condition,
    TestLabel "select_anymatch" test_select_anymatch,
    TestLabel "select_nonematch" test_select_nonematch
  ])



-- | Main | --

tests = TestList [jsonOutputTests, jsonTransformerTests]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 || errors result > 0
    then exitFailure
    else putStrLn "All tests passed"
