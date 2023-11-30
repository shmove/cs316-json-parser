{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module QueryLanguage where

import JSONTransformer
import ParserCombinators

data Query
  = Pipe           Query Query
  | Concat         Query Query
  | Field          String
  | Elements
  | Select         Query
  | ConstString    String
  | ConstBool      Bool
  | ConstNull
  | ConstInt       Int
  | Equal          Query Query
  | NotEqual       Query Query
  | LessThan       Query Query
  | LessOrEqual    Query Query
  | GreaterThan    Query Query
  | GreaterOrEqual Query Query
  | And            Query Query
  | Or             Query Query
  | Not            Query
  | None
  deriving Show

-- | Executes a 'Query' by translating it into a `Transformer`. Each
-- of the constructors of 'Query' is turned into its corresponding
-- `Transformer` defined in `JSONTransformer`.
--
-- For example:
--
-- >  execute Elements (Array [Number 1, Number 2])
--
-- returns
--
-- >  [Number 1, Number 2]
--
-- which is the behaviour of `elements` on this input.
execute :: Query -> Transformer
execute (Pipe q1 q2)    = pipe (execute q1) (execute q2)
execute (Concat q1 q2)  = concatenate (execute q1) (execute q2)
execute (Field str)     = field str
execute Elements        = elements
execute (Select q)      = select (execute q)
execute (ConstString s) = string s
execute (ConstBool b)   = bool b
execute ConstNull       = tNull
execute (ConstInt n)    = int n
execute (Equal q1 q2)          = comparison (==) (execute q1) (execute q2)
execute (NotEqual q1 q2)       = comparison (/=) (execute q1) (execute q2)
execute (LessThan q1 q2)       = comparison (<)  (execute q1) (execute q2)
execute (LessOrEqual q1 q2)    = comparison (<=) (execute q1) (execute q2)
execute (GreaterThan q1 q2)    = comparison (>)  (execute q1) (execute q2)
execute (GreaterOrEqual q1 q2) = comparison (>=) (execute q1) (execute q2)
execute (And q1 q2)            = tAnd (execute q1) (execute q2)
execute (Or q1 q2)             = tOr  (execute q1) (execute q2)
execute (Not q)                = tNot (execute q)
execute None = identity

-- HINT: this function is very similar to the 'eval' function for
-- evaluating Boolean formulas defined in the Week03 problems.

-- | Query Parsing | --

-- | Parses a query. A query is a complex query expression surrounded by whitespace.
parseQuery :: Parser Query
parseQuery =
  do whitespaces
     q <- parseCombinatoryQueryExpr
     whitespaces
     return q

parseCombinatoryQueryExpr :: Parser Query
parseCombinatoryQueryExpr =
  do (q1, q2) <- parseCombinatoryExpr "|"
     whitespaces
     return (Pipe q1 q2)
  `orElse`
  do (q1, q2) <- parseCombinatoryExpr ","
     whitespaces
     return (Concat q1 q2)
  `orElse`
  do parseConditionalQueryExpr

parseConditionalQueryExpr :: Parser Query
parseConditionalQueryExpr =
  do (q1, q2) <- parseConditionalExpr "and"
     whitespaces
     return (And q1 q2)
  `orElse`
  do (q1, q2) <- parseConditionalExpr "&&"
     whitespaces
     return (And q1 q2)
  `orElse`
  do (q1, q2) <- parseConditionalExpr "or"
     whitespaces
     return (Or q1 q2)
  `orElse`
  do (q1, q2) <- parseConditionalExpr "||"
     whitespaces
     return (Or q1 q2)
  `orElse`
  do parseComparisonQueryExpr

-- | Parses a comparison query expression. A comparison query expression is one of the following:
-- - A query expression that splits into a base query expression and a comparison query expression, separated by some operator.
-- - A base query expression.
parseComparisonQueryExpr :: Parser Query
parseComparisonQueryExpr =
  do (q1, q2) <- parseComparisonExpr "=="
     whitespaces
     return (Equal q1 q2)
  `orElse`
  do (q1, q2) <- parseComparisonExpr "!="
     whitespaces
     return (NotEqual q1 q2)
  `orElse`
  do (q1, q2) <- parseComparisonExpr "<"
     whitespaces
     return (LessThan q1 q2) 
  `orElse`
  do (q1, q2) <- parseComparisonExpr "<="
     whitespaces
     return (LessOrEqual q1 q2) 
  `orElse`
  do (q1, q2) <- parseComparisonExpr ">"
     whitespaces
     return (GreaterThan q1 q2) 
  `orElse`
  do (q1, q2) <- parseComparisonExpr ">="
     whitespaces
     return (GreaterOrEqual q1 q2) 
  `orElse`
  do parseBaseQueryExpr

-- | Parses a base query expression. A base query expression is any query expression that is contiguous and not separated by any operators.
parseBaseQueryExpr :: Parser Query
parseBaseQueryExpr =
  do stringLiteral "not"
     whitespaces
     q <- parseBaseQueryExpr
     return (Not q)
  `orElse`
  do stringLiteral "!"
     whitespaces
     q <- parseBaseQueryExpr
     return (Not q)
  `orElse`
  do q <- parseBrackets
     whitespaces
     return q
  `orElse`
  do q <- parseSelect
     whitespaces
     return q
  `orElse`
  do q <- parseArrayItems
     whitespaces
     return q
  `orElse`
  do q <- parseChainedField
     whitespaces
     return q
  `orElse`
  do q <- parseConst
     whitespaces
     return q
  `orElse`
  do failParse "Invalid query structure!"

-- 
parseCombinatoryExpr :: String -> Parser (Query, Query)
parseCombinatoryExpr c =
  do q1 <- parseConditionalQueryExpr
     whitespaces
     stringLiteral c
     whitespaces
     q2 <- parseCombinatoryQueryExpr
     return (q1, q2)

-- 
parseConditionalExpr :: String -> Parser (Query, Query)
parseConditionalExpr c =
   do q1 <- parseComparisonQueryExpr
      whitespaces
      stringLiteral c
      whitespaces
      q2 <- parseConditionalQueryExpr
      return (q1, q2)

-- | Parses a comparison expression. A comparison expression is a comparison query expression that is split into
-- a base query expression and a comparison query expression, separated by some operator.
parseComparisonExpr :: String -> Parser (Query, Query)
parseComparisonExpr c =
   do q1 <- parseBaseQueryExpr
      whitespaces
      stringLiteral c
      whitespaces
      q2 <- parseComparisonQueryExpr
      return (q1, q2)

-- | Parses a bracketed query expression. A bracketed query expression is a query expression surrounded by brackets '(' and ')'.
parseBrackets :: Parser Query
parseBrackets =
  do isChar '('
     whitespaces
     q <- parseCombinatoryQueryExpr
     whitespaces
     isChar ')'
     return q

-- | Parses a select expression. A select expression is the keyword 'select' followed by a bracketed query expression.
parseSelect :: Parser Query
parseSelect =
  do stringLiteral "select"
     whitespaces
     isChar '('
     whitespaces
     q <- parseCombinatoryQueryExpr
     whitespaces
     isChar ')'
     return (Select q)

-- | Parses chained fields. A chained field is a sequence of fields separated by the dot character '.'.
--
-- For example:
--
-- >  parseChainedField "a.b.c"
--
-- returns
--
-- >  Pipe (Field "a") (Pipe (Field "b") (Field "c"))
parseChainedField :: Parser Query
parseChainedField =
  do field <- parseField
     chainedField <- parseChainedField
     return (Pipe field chainedField)
  `orElse`
  do parseField

-- | Parses a field name. A field name is a non-empty sequence of non whitespace / disallowed characters. 
--
-- A field name can also be a quoted string to allow for whitespace, dots and brackets.
-- 
-- If the field name is given as '[]' then it is parsed as the 'Elements' query.
parseField :: Parser Query
parseField =
   do isChar '.'
      isChar '['
      whitespaces
      isChar ']'
      return Elements
   `orElse`
   do isChar '.'
      fstr <- quotedString
      return (Field fstr)
   `orElse`
   do isChar '.'
      field <- oneOrMore (satisfies "non-whitespace or query character" (\c -> c /= ' ' && c /= '\n' && c /= '\t' && c /= '.' && c /= '[' && c /= ']' && c /= ',' && c /= '(' && c /= ')'))
      return (Field field)
   `orElse`
   do isChar '.'
      return None

-- | Parses a query constant.
parseConst :: Parser Query
parseConst =
  do b <- parseBool
     return (ConstBool b)
  `orElse`
  do stringLiteral "null"
     return ConstNull
  `orElse`
  do str <- quotedString
     return (ConstString str)
  `orElse`
  do num <- number
     return (ConstInt num)

-- | Parses a boolean value. A boolean value is either 'true' or 'false' (first letter case insensitive).
parseBool :: Parser Bool
parseBool =
  do stringLiteral "true"
     return True
  `orElse`
  do stringLiteral "True"
     return True
  `orElse`
  do stringLiteral "false"
     return False
  `orElse`
  do stringLiteral "False"
     return False

-- | Parses an array of queries. An array of queries is a sequence of query expressions separated by commas ',' and surrounded by square brackets '[' and ']'.
parseArrayItems :: Parser Query
parseArrayItems =
   do isChar '['
      whitespaces
      xs <- parseCombinatoryQueryExpr
      whitespaces
      isChar ']'
      return xs
