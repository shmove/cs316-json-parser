import System.Exit
import Test.QuickCheck ()
import Test.HUnit

import JSON
import JSONOutput
import JSONTransformer
import Result
import QueryLanguage
import ParserCombinators

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



-- | JSON.hs | --

-- Returns whether a JSON value is truthy or falsy. Falsy values are False and Null.
test_getTruthy_truthy = TestCase (assertEqual ("for getTruthy (Boolean True)") True (getTruthy (Boolean True)))
test_getTruthy_truthy_any = TestCase (assertEqual ("for getTruthy (Number 1)") True (getTruthy (Number 1)))
test_getTruthy_falsy = TestCase (assertEqual ("for getTruthy (Boolean False)") False (getTruthy (Boolean False)))
test_getTruthy_falsy_null = TestCase (assertEqual ("for getTruthy Null") False (getTruthy Null))

jsonAdditionalTests = TestLabel "JSON Additional Tests" (TestList
  [
    TestLabel "getTruthy_truthy" test_getTruthy_truthy,
    TestLabel "getTruthy_truthy_any" test_getTruthy_truthy_any,
    TestLabel "getTruthy_falsy" test_getTruthy_falsy,
    TestLabel "getTruthy_falsy_null" test_getTruthy_falsy_null
  ])



-- | JSONTransformer.hs | --

-- Ignores JSON input and returns the given string as a piece of JSON in a one element list.
test_string = TestCase (assertEqual ("for string \"hello\" (Number 1)") (Ok [String "hello"]) (string "hello" (Number 1)))

-- Ignores JSON input and returns the given integer as a piece of JSON in a one element list.
test_int = TestCase (assertEqual ("for int 1234 (Number 1)") (Ok [Number 1234]) (int 1234 (Number 1)))

-- Returns all the elements of a JSON Array. If the JSON value is not an Array, then it returns an error.
test_elements = TestCase (assertEqual ("for elements (Array [Number 1, Number 2, Boolean True])") (Ok [Number 1, Number 2, Boolean True]) (elements (Array [Number 1, Number 2, Boolean True])))
test_elements_invalid = TestCase (assertEqual ("for elements (Number 1)") (Error "Not an array") (elements (Number 1)))

-- Looks up a field in a JSON Object and returns the value associated with that field in a one element list. If the JSON value is not an Object, or the field does not exist, then it returns an error.
test_field = TestCase (assertEqual ("for field \"a\" (Object [(\"a\", Number 1)])") (Ok [Number 1]) (field "a" (Object [("a", Number 1)])))
test_field_invalid = TestCase (assertEqual ("for field \"a\" (Number 1)") (Error "Field 'a' not found / Not an object") (field "a" (Number 1)))
test_field_nonexistent = TestCase (assertEqual ("for field \"b\" (Object [(\"a\", Number 1)])") (Error "Field 'b' not found / Not an object") (field "b" (Object [("a", Number 1)])))

-- Connecting elements to elements via a pipe unwraps two levels of arrays
test_pipe_unwrap = TestCase (assertEqual ("for pipe elements elements (Array [Array [Number 1,Number 2], Array [Number 3,Number 4]])") (Ok [Number 1,Number 2,Number 3,Number 4]) (pipe elements elements (Array [Array [Number 1,Number 2], Array [Number 3,Number 4]])))

-- Compares all pairs of values from the two transformed lists for equality, returning 'Boolean True' or 'Boolean False' for each one.
test_comparison_truthy = TestCase (assertEqual ("for comparison (==) (string \"a\") (string \"a\") (Number 1)") (Ok [Boolean True]) (comparison (==) (string "a") (string "a") (Number 1)))
test_comparison_falsy = TestCase (assertEqual ("for comparison (==) (string \"a\") (string \"b\") (Number 1)") (Ok [Boolean False]) (comparison (==) (string "a") (string "b") (Number 1)))

-- Filter the input. If the transformer argument returns 'true' for the input then return the input in a single element list, else return the empty list.
test_select_match_condition = TestCase (assertEqual ("for select (equal (field \"a\") (int 1)) (Object [(\"a\",Number 1)])") (Ok [Object [("a",Number 1)]]) (select (comparison (==) (field "a") (int 1)) (Object [("a",Number 1)])))
test_select_nomatch_condition = TestCase (assertEqual ("for select (equal (field \"a\") (int 1)) (Object [(\"a\",Number 2)])") (Ok []) (select (comparison (==) (field "a") (int 1)) (Object [("a",Number 2)])))
test_select_anymatch = TestCase (assertEqual ("for select (equal elements (int 1)) (Array [Number 1, Number 3, Number 4])") (Ok [Array [Number 1,Number 3,Number 4]]) (select (comparison (==) elements (int 1)) (Array [Number 1, Number 3, Number 4])))
test_select_nonematch = TestCase (assertEqual ("for select (equal elements (int 1)) (Array [Number 3, Number 4])") (Ok []) (select (comparison (==) elements (int 1)) (Array [Number 3, Number 4])))

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
    TestLabel "equal_truthy" test_comparison_truthy,
    TestLabel "equal_falsy" test_comparison_falsy,
    TestLabel "select_match_condition" test_select_match_condition,
    TestLabel "select_nomatch_condition" test_select_nomatch_condition,
    TestLabel "select_anymatch" test_select_anymatch,
    TestLabel "select_nonematch" test_select_nonematch
  ])



-- Ignores JSON input and returns the given boolean as a piece of JSON in a one element list.
test_bool_true = TestCase (assertEqual ("for bool True (Number 1)") (Ok [Boolean True]) (bool True (Number 1)))
test_bool_false = TestCase (assertEqual ("for bool False (Number 1)") (Ok [Boolean False]) (bool False (Number 1)))

-- Ignores JSON input and returns Null as a piece of JSON in a one element list.
test_tNull = TestCase (assertEqual ("for tNull (Number 1)") (Ok [Null]) (tNull (Number 1)))

-- Concatenates the results of two transformers into a single list.
test_concatenate = TestCase (assertEqual ("for concatenate (string \"a\") (string \"b\") (Number 1)") (Ok [String "a", String "b"]) (concatenate (string "a") (string "b") (Number 1)))

-- Returns the input unchanged.
test_identity = TestCase (assertEqual ("for identity (Number 1)") (Ok [Number 1]) (identity (Number 1)))

-- Returns whether both transformers evaluate to a truthy value.
test_tAnd_truthy = TestCase (assertEqual ("for tAnd (bool True) (bool True) (Number 1)") (Ok [Boolean True]) (tAnd (bool True) (bool True) (Number 1)))
test_tAnd_falsy = TestCase (assertEqual ("for tAnd (bool True) (bool False) (Number 1)") (Ok [Boolean False]) (tAnd (bool True) (bool False) (Number 1)))
test_tAnd_falsy_null = TestCase (assertEqual ("for tAnd (bool True) tNull (Number 1)") (Ok [Boolean False]) (tAnd (bool True) tNull (Number 1)))

-- Returns whether either transformer evaluates to a truthy value.
test_tOr_truthy = TestCase (assertEqual ("for tOr (bool True) (bool False) (Number 1)") (Ok [Boolean True]) (tOr (bool True) (bool False) (Number 1)))
test_tOr_falsy = TestCase (assertEqual ("for tOr (bool False) (bool False) (Number 1)") (Ok [Boolean False]) (tOr (bool False) (bool False) (Number 1)))
test_tOr_truthy_null = TestCase (assertEqual ("for tOr (bool True) tNull (Number 1)") (Ok [Boolean True]) (tOr (bool True) tNull (Number 1)))

-- Returns the opposite of the transformer's truthy value.
test_tNot_truthy = TestCase (assertEqual ("for tNot (bool True) (Number 1)") (Ok [Boolean False]) (tNot (bool True) (Number 1)))
test_tNot_falsy = TestCase (assertEqual ("for tNot (bool False) (Number 1)") (Ok [Boolean True]) (tNot (bool False) (Number 1)))
test_tNot_falsy_null = TestCase (assertEqual ("for tNot tNull (Number 1)") (Ok [Boolean True]) (tNot tNull (Number 1)))

-- Returns the result of an addition operation on the two transformers' results.
test_tAdd_num = TestCase (assertEqual ("for tAdd (int 1) (int 2) (Number 1)") (Ok [Number 3]) (tAdd (int 1) (int 2) (Number 1)))
test_tAdd_null = TestCase (assertEqual ("for tAdd (int 1) tNull (Number 1)") (Ok [Number 1]) (tAdd (int 1) tNull (Number 1)))
test_tAdd_null2 = TestCase (assertEqual ("for tAdd tNull (int 1) (Number 1)") (Ok [Number 1]) (tAdd tNull (int 1) (Number 1)))
test_tAdd_array = TestCase (assertEqual ("for tAdd (Array [Number 1]) (Array [Number 2]) (Number 1)") (Ok [Array [Number 1, Number 1]]) (tAdd identity identity (Array [Number 1])))
test_tAdd_string = TestCase (assertEqual ("for tAdd (string \"a\") (string \"b\") (Number 1)") (Ok [String "ab"]) (tAdd (string "a") (string "b") (Number 1)))
test_tAdd_unsupported = TestCase (assertEqual ("for tAdd (bool True) (bool False) (Number 1)") (Error "Invalid addition operation") (tAdd (bool True) (bool False) (Number 1)))

-- Returns the result of a subtraction operation on the two transformers' results.
test_tSub_num = TestCase (assertEqual ("for tSub (int 3) (int 2) (Number 1)") (Ok [Number 1]) (tSub (int 3) (int 2) (Number 1)))
test_tSub_unsupported = TestCase (assertEqual ("for tSub (bool True) (bool False) (Number 1)") (Error "Invalid subtraction operation") (tSub (bool True) (bool False) (Number 1)))

-- Returns the result of a multiplication operation on the two transformers' results.
test_tMul_num = TestCase (assertEqual ("for tMul (int 3) (int 2) (Number 1)") (Ok [Number 6]) (tMul (int 3) (int 2) (Number 1)))
test_tMul_string = TestCase (assertEqual ("for tMul (int 3) (string \"hi\") (Number 1)") (Ok [String "hihihi"]) (tMul (int 3) (string "hi") (Number 1)))
test_tMul_string_2 = TestCase (assertEqual ("for tMul (string \"hi\") (int 3) (Number 1)") (Ok [String "hihihi"]) (tMul (string "hi") (int 3) (Number 1)))
test_tMul_unsupported = TestCase (assertEqual ("for tMul (bool True) (bool False) (Number 1)") (Error "Invalid multiplication operation") (tMul (bool True) (bool False) (Number 1)))

-- Returns the result of a division operation on the two transformers' results.
test_tDiv_num = TestCase (assertEqual ("for tDiv (int 6) (int 2) (Number 1)") (Ok [Number 3]) (tDiv (int 6) (int 2) (Number 1)))
test_tDiv_unsupported = TestCase (assertEqual ("for tDiv (bool True) (bool False) (Number 1)") (Error "Invalid division operation") (tDiv (bool True) (bool False) (Number 1)))

-- Returns the result of a modulo operation on the two transformers' results.
test_tMod_num = TestCase (assertEqual ("for tMod (int 5) (int 2) (Number 1)") (Ok [Number 1]) (tMod (int 5) (int 2) (Number 1)))
test_tMod_unsupported = TestCase (assertEqual ("for tMod (bool True) (bool False) (Number 1)") (Error "Invalid modulus operation") (tMod (bool True) (bool False) (Number 1)))

jsonTransformerAdditionalTests = TestLabel "JSONTransformer Additional Tests" (TestList
  [
    TestLabel "bool_true" test_bool_true,
    TestLabel "bool_false" test_bool_false,
    TestLabel "tNull" test_tNull,
    TestLabel "concatenate" test_concatenate,
    TestLabel "identity" test_identity,
    TestLabel "tAnd_truthy" test_tAnd_truthy,
    TestLabel "tAnd_falsy" test_tAnd_falsy,
    TestLabel "tAnd_falsy_null" test_tAnd_falsy_null,
    TestLabel "tOr_truthy" test_tOr_truthy,
    TestLabel "tOr_falsy" test_tOr_falsy,
    TestLabel "tOr_truthy_null" test_tOr_truthy_null,
    TestLabel "tNot_truthy" test_tNot_truthy,
    TestLabel "tNot_falsy" test_tNot_falsy,
    TestLabel "tNot_falsy_null" test_tNot_falsy_null,
    TestLabel "tAdd_num" test_tAdd_num,
    TestLabel "tAdd_null" test_tAdd_null,
    TestLabel "tAdd_null2" test_tAdd_null2,
    TestLabel "tAdd_array" test_tAdd_array,
    TestLabel "tAdd_string" test_tAdd_string,
    TestLabel "tAdd_unsupported" test_tAdd_unsupported,
    TestLabel "tSub_num" test_tSub_num,
    TestLabel "tSub_unsupported" test_tSub_unsupported,
    TestLabel "tMul_num" test_tMul_num,
    TestLabel "tMul_string" test_tMul_string,
    TestLabel "tMul_string_2" test_tMul_string_2,
    TestLabel "tMul_unsupported" test_tMul_unsupported,
    TestLabel "tDiv_num" test_tDiv_num,
    TestLabel "tDiv_unsupported" test_tDiv_unsupported,
    TestLabel "tMod_num" test_tMod_num,
    TestLabel "tMod_unsupported" test_tMod_unsupported
  ])


-- | QueryLanguage.hs | --

-- Executes a 'Query' by translating it into a `Transformer`. Each
-- of the constructors of 'Query' is turned into its corresponding
-- `Transformer` defined in `JSONTransformer`.
test_execute_pipe = TestCase (assertEqual ("for execute (Pipe None None) (Number 1)") (Ok [Number 1]) (execute (Pipe None None) (Number 1)))
test_execute_concat = TestCase (assertEqual ("for execute (Concat None None) (Number 1)") (Ok [Number 1, Number 1]) (execute (Concat None None) (Number 1)))
test_execute_field = TestCase (assertEqual ("for execute (Field \"a\") (Object [(\"a\", Number 1)])") (Ok [Number 1]) (execute (Field "a") (Object [("a", Number 1)])))
test_execute_elements = TestCase (assertEqual ("for execute Elements (Array [Number 1, Number 2, Number 3])") (Ok [Number 1, Number 2, Number 3]) (execute Elements (Array [Number 1, Number 2, Number 3])))
test_execute_select = TestCase (assertEqual ("for execute (Select (Equal (Field \"a\") (ConstInt 1))) (Object [(\"a\", Number 1)])") (Ok [Object [("a", Number 1)]]) (execute (Select (Equal (Field "a") (ConstInt 1))) (Object [("a", Number 1)])))
test_execute_string = TestCase (assertEqual ("for execute (ConstString \"hello\") (Number 1)") (Ok [String "hello"]) (execute (ConstString "hello") (Number 1)))
test_execute_bool = TestCase (assertEqual ("for execute (ConstBool True) (Number 1)") (Ok [Boolean True]) (execute (ConstBool True) (Number 1)))
test_execute_null = TestCase (assertEqual ("for execute ConstNull (Number 1)") (Ok [Null]) (execute ConstNull (Number 1)))
test_execute_int = TestCase (assertEqual ("for execute (ConstInt 1234) (Number 1)") (Ok [Number 1234]) (execute (ConstInt 1234) (Number 1)))
test_execute_equal = TestCase (assertEqual ("for execute (Equal (ConstInt 1) (ConstInt 1)) (Number 1)") (Ok [Boolean True]) (execute (Equal (ConstInt 1) (ConstInt 1)) (Number 1)))
test_execute_and = TestCase (assertEqual ("for execute (And (ConstBool True) (ConstBool True)) (Number 1)") (Ok [Boolean True]) (execute (And (ConstBool True) (ConstBool True)) (Number 1)))
test_execute_or = TestCase (assertEqual ("for execute (Or (ConstBool True) (ConstBool False)) (Number 1)") (Ok [Boolean True]) (execute (Or (ConstBool True) (ConstBool False)) (Number 1)))
test_execute_not = TestCase (assertEqual ("for execute (Not (ConstBool True)) (Number 1)") (Ok [Boolean False]) (execute (Not (ConstBool True)) (Number 1)))
test_execute_add = TestCase (assertEqual ("for execute (Add (ConstInt 1) (ConstInt 2)) (Number 1)") (Ok [Number 3]) (execute (Add (ConstInt 1) (ConstInt 2)) (Number 1)))
test_execute_sub = TestCase (assertEqual ("for execute (Subtract (ConstInt 3) (ConstInt 2)) (Number 1)") (Ok [Number 1]) (execute (Subtract (ConstInt 3) (ConstInt 2)) (Number 1)))
test_execute_mul = TestCase (assertEqual ("for execute (Multiply (ConstInt 3) (ConstInt 2)) (Number 1)") (Ok [Number 6]) (execute (Multiply (ConstInt 3) (ConstInt 2)) (Number 1)))
test_execute_div = TestCase (assertEqual ("for execute (Divide (ConstInt 6) (ConstInt 2)) (Number 1)") (Ok [Number 3]) (execute (Divide (ConstInt 6) (ConstInt 2)) (Number 1)))
test_execute_mod = TestCase (assertEqual ("for execute (Modulo (ConstInt 5) (ConstInt 2)) (Number 1)") (Ok [Number 1]) (execute (Modulo (ConstInt 5) (ConstInt 2)) (Number 1)))
text_execute_none = TestCase (assertEqual ("for execute None (Number 1)") (Ok [Number 1]) (execute None (Number 1)))

queryLanguageExecuteTests = TestLabel "QueryLanguage Execute Tests" (TestList
  [
    TestLabel "execute_pipe" test_execute_pipe,
    TestLabel "execute_concat" test_execute_concat,
    TestLabel "execute_field" test_execute_field,
    TestLabel "execute_elements" test_execute_elements,
    TestLabel "execute_select" test_execute_select,
    TestLabel "execute_string" test_execute_string,
    TestLabel "execute_bool" test_execute_bool,
    TestLabel "execute_null" test_execute_null,
    TestLabel "execute_int" test_execute_int,
    TestLabel "execute_equal" test_execute_equal,
    TestLabel "execute_and" test_execute_and,
    TestLabel "execute_or" test_execute_or,
    TestLabel "execute_not" test_execute_not,
    TestLabel "execute_add" test_execute_add,
    TestLabel "execute_sub" test_execute_sub,
    TestLabel "execute_mul" test_execute_mul,
    TestLabel "execute_div" test_execute_div,
    TestLabel "execute_mod" test_execute_mod,
    TestLabel "execute_none" text_execute_none
  ])

test_query_parse_const_bool1 = TestCase (assertEqual ("for runParser parseQuery \"true\"") (Ok (ConstBool True, "")) (runParser parseQuery "true"))
test_query_parse_const_bool2 = TestCase (assertEqual ("for runParser parseQuery \"True\"") (Ok (ConstBool True, "")) (runParser parseQuery "True"))
test_query_parse_const_bool3 = TestCase (assertEqual ("for runParser parseQuery \"false\"") (Ok (ConstBool False, "")) (runParser parseQuery "false"))
test_query_parse_const_bool4 = TestCase (assertEqual ("for runParser parseQuery \"False\"") (Ok (ConstBool False, "")) (runParser parseQuery "False"))
test_query_parse_const_null = TestCase (assertEqual ("for runParser parseQuery \"null\"") (Ok (ConstNull, "")) (runParser parseQuery "null"))
test_query_parse_const_string = TestCase (assertEqual ("for runParser parseQuery \"\\\"hello\\\"\"") (Ok (ConstString "hello", "")) (runParser parseQuery "\"hello\""))
test_query_parse_const_number = TestCase (assertEqual ("for runParser parseQuery \"1\"") (Ok (ConstInt 1, "")) (runParser parseQuery "1"))
test_query_parse_field = TestCase (assertEqual ("for runParser parseQuery \".a\"") (Ok (Field "a", "")) (runParser parseQuery ".a"))
test_query_parse_chained_field = TestCase (assertEqual ("for runParser parseQuery \".a.b.c\"") (Ok (Pipe (Field "a") (Pipe (Field "b") (Field "c")), "")) (runParser parseQuery ".a.b.c"))
-- test_query_parse_array = TestCase (assertEqual ("for runParser parseQuery \"[1, 2]\"") (Ok ((Concat (ConstInt 1) (ConstInt 2)), "")) (runParser parseQuery "[1, 2]"))
test_query_parse_select = TestCase (assertEqual ("for runParser parseQuery \"select (.a == 1)\"") (Ok ((Select (Equal (Field "a") (ConstInt 1))), "")) (runParser parseQuery "select (.a == 1)"))
test_query_parse_brackets = TestCase (assertEqual ("for runParser parseQuery \"(true)\"") (Ok ((ConstBool True), "")) (runParser parseQuery "(true)"))
test_query_parse_not = TestCase (assertEqual ("for runParser parseQuery \"not true\"") (Ok ((Not (ConstBool True)), "")) (runParser parseQuery "not true"))
test_query_parse_not2 = TestCase (assertEqual ("for runParser parseQuery \"! true\"") (Ok ((Not (ConstBool True)), "")) (runParser parseQuery "! true"))
test_query_parse_math = TestCase (assertEqual ("for runParser parseQuery \"2 * 10 / 2 * (3 - 1) + 5\"") (Ok (Add (Multiply (ConstInt 2) (Multiply (Divide (ConstInt 10) (ConstInt 2)) (Subtract (ConstInt 3) (ConstInt 1)))) (ConstInt 5),"")) (runParser parseQuery "2 * 10 / 2 * (3 - 1) + 5"))
test_query_parse_greaterorequal = TestCase (assertEqual ("for runParser parseQuery \"1 >= 2\"") (Ok ((GreaterOrEqual (ConstInt 1) (ConstInt 2)), "")) (runParser parseQuery "1 >= 2"))
test_query_parse_greaterthan = TestCase (assertEqual ("for runParser parseQuery \"1 > 2\"") (Ok ((GreaterThan (ConstInt 1) (ConstInt 2)), "")) (runParser parseQuery "1 > 2"))
test_query_parse_lessthanorequal = TestCase (assertEqual ("for runParser parseQuery \"1 <= 2\"") (Ok ((LessOrEqual (ConstInt 1) (ConstInt 2)), "")) (runParser parseQuery "1 <= 2"))
test_query_parse_lessthan = TestCase (assertEqual ("for runParser parseQuery \"1 < 2\"") (Ok ((LessThan (ConstInt 1) (ConstInt 2)), "")) (runParser parseQuery "1 < 2"))
test_query_parse_notequal = TestCase (assertEqual ("for runParser parseQuery \"1 != 1\"") (Ok ((NotEqual (ConstInt 1) (ConstInt 1)), "")) (runParser parseQuery "1 != 1"))
test_query_parse_equal = TestCase (assertEqual ("for runParser parseQuery \"1 == 1\"") (Ok ((Equal (ConstInt 1) (ConstInt 1)), "")) (runParser parseQuery "1 == 1"))
test_query_parse_equal2 = TestCase (assertEqual ("for runParser parseQuery \"1 == 1\"") (Ok ((Equal (ConstInt 1) (ConstInt 1)), "")) (runParser parseQuery "1 == 1"))
test_query_parse_or = TestCase (assertEqual ("for runParser parseQuery \"true or false\"") (Ok ((Or (ConstBool True) (ConstBool False)), "")) (runParser parseQuery "true or false"))
test_query_parse_or2 = TestCase (assertEqual ("for runParser parseQuery \"true || false\"") (Ok ((Or (ConstBool True) (ConstBool False)), "")) (runParser parseQuery "true || false"))
test_query_parse_and = TestCase (assertEqual ("for runParser parseQuery \"true and false\"") (Ok ((And (ConstBool True) (ConstBool False)), "")) (runParser parseQuery "true and false"))
test_query_parse_and2 = TestCase (assertEqual ("for runParser parseQuery \"true && false\"") (Ok ((And (ConstBool True) (ConstBool False)), "")) (runParser parseQuery "true && false"))
test_query_parse_concat = TestCase (assertEqual ("for runParser parseQuery \"., .\"") (Ok ((Concat None None), "")) (runParser parseQuery "., ."))
test_query_parse_pipe = TestCase (assertEqual ("for runParser parseQuery \". | .\"") (Ok ((Pipe None None), "")) (runParser parseQuery ". | ."))
test_query_none = TestCase (assertEqual ("for runParser parseQuery \"\"") (Error "Invalid query structure!") (runParser parseQuery ""))

queryLanguageParseTests = TestLabel "QueryLanguage Parse Tests" (TestList
  [
    TestLabel "query_parse_const_bool1" test_query_parse_const_bool1,
    TestLabel "query_parse_const_bool2" test_query_parse_const_bool2,
    TestLabel "query_parse_const_bool3" test_query_parse_const_bool3,
    TestLabel "query_parse_const_bool4" test_query_parse_const_bool4,
    TestLabel "query_parse_const_null" test_query_parse_const_null,
    TestLabel "query_parse_const_string" test_query_parse_const_string,
    TestLabel "query_parse_const_number" test_query_parse_const_number,
    TestLabel "query_parse_field" test_query_parse_field,
    TestLabel "query_parse_chained_field" test_query_parse_chained_field,
    -- TestLabel "query_parse_array" test_query_parse_array,
    TestLabel "query_parse_select" test_query_parse_select,
    TestLabel "query_parse_brackets" test_query_parse_brackets,
    TestLabel "query_parse_not" test_query_parse_not,
    TestLabel "query_parse_not2" test_query_parse_not2,
    TestLabel "query_parse_math" test_query_parse_math,
    TestLabel "query_parse_greaterorequal" test_query_parse_greaterorequal,
    TestLabel "query_parse_greaterthan" test_query_parse_greaterthan,
    TestLabel "query_parse_lessthanorequal" test_query_parse_lessthanorequal,
    TestLabel "query_parse_lessthan" test_query_parse_lessthan,
    TestLabel "query_parse_notequal" test_query_parse_notequal,
    TestLabel "query_parse_equal" test_query_parse_equal,
    TestLabel "query_parse_equal2" test_query_parse_equal2,
    TestLabel "query_parse_or" test_query_parse_or,
    TestLabel "query_parse_or2" test_query_parse_or2,
    TestLabel "query_parse_and" test_query_parse_and,
    TestLabel "query_parse_and2" test_query_parse_and2,
    TestLabel "query_parse_concat" test_query_parse_concat,
    TestLabel "query_parse_pipe" test_query_parse_pipe,
    TestLabel "query_none" test_query_none
  ])

-- | Main | --

tests = TestList [jsonOutputTests, jsonAdditionalTests, jsonTransformerTests, jsonTransformerAdditionalTests, queryLanguageExecuteTests, queryLanguageParseTests]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 || errors result > 0
    then exitFailure
    else putStrLn "All tests passed"
