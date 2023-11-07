module JSONOutput where

import JSON
import Data.List (intersperse, intercalate)

-- | Converts a 'String' into its string JSON representation by
-- placing quotes around it and escaping any quote characters that
-- appear in the string.
--
-- For example,
--
-- >  renderString "hello \"world\""
--
-- Returns
--
-- >  "\"hello \\\"world\\\"\""
renderString :: String -> String
renderString xs = "\"" ++ escapeString xs ++ "\""

escapeString :: String -> String
escapeString [] = []
escapeString ('"':xs)  = "\\\"" ++ escapeString xs
escapeString ('\\':xs) = "\\\\" ++ escapeString xs
escapeString ('/':xs)  = "\\/" ++ escapeString xs
escapeString ('\b':xs) = "\\b" ++ escapeString xs
escapeString ('\f':xs) = "\\f" ++ escapeString xs
escapeString ('\n':xs) = "\\n" ++ escapeString xs
escapeString ('\r':xs) = "\\r" ++ escapeString xs
escapeString ('\t':xs) = "\\t" ++ escapeString xs
-- hex chars?
escapeString (c:xs)    = c : escapeString xs

-- HINT: one way of doing the escaping is the same way that we did the
-- htmlEscape function in the Week01 problems.

-- | Converts `JSON` into its string representation, as documented in
-- the JSON standard (https://www.json.org/json-en.html). The format
-- should be as is understood by the parsing functions in `JSONInput`.
--
-- For example,
--
--  > renderJSON (Object [("a",Number 1), ("b",Number 2)])
--
-- should give
--
--  > "{\"a\": 1, \"b\": 2}"
renderJSON :: JSON -> String
renderJSON (String s)  = renderString s
renderJSON (Boolean b) = if b then "true" else "false"
renderJSON Null        = "null"
renderJSON (Number n)  = show n
renderJSON (Array a)   = "[" ++ intercalate ", " (map renderJSON a) ++ "]"
renderJSON (Object o)  = "{" ++ intercalate ", " (parseJSONObject o) ++ "}"

parseJSONObject :: [(String, JSON)] -> [String]
parseJSONObject [] = []
parseJSONObject ((s, j):xs) = (renderString s ++ ": " ++ renderJSON j) : parseJSONObject xs

-- HINT: the `intersperse` function (imported above) is a good way of
-- putting something between every element of a list. It is the
-- "printing" counterpart of the 'sepBy' parser combinator.
--
-- You should use 'renderString' to implement rendering of 'String's
-- in the JSON.
