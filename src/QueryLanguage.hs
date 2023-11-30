{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module QueryLanguage where

import JSONTransformer
import ParserCombinators

data Query
  = Pipe        Query Query
  | Concat      Query Query
  | Field       String
  | Elements
  | Select      Query
  | ConstBool   Bool
  | ConstInt    Int
  | ConstString String
  | Equal          Query Query
  | NotEqual       Query Query
  | LessThan       Query Query
  | LessOrEqual    Query Query
  | GreaterThan    Query Query
  | GreaterOrEqual Query Query
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
execute (ConstBool b)   = bool b
execute (ConstInt n)    = int n
execute (ConstString s) = string s
execute (Equal q1 q2)          = comparison (==) (execute q1) (execute q2)
execute (NotEqual q1 q2)       = comparison (/=) (execute q1) (execute q2)
execute (LessThan q1 q2)       = comparison (<)  (execute q1) (execute q2)
execute (LessOrEqual q1 q2)    = comparison (<=) (execute q1) (execute q2)
execute (GreaterThan q1 q2)    = comparison (>)  (execute q1) (execute q2)
execute (GreaterOrEqual q1 q2) = comparison (>=) (execute q1) (execute q2)
execute None = identity

-- HINT: this function is very similar to the 'eval' function for
-- evaluating Boolean formulas defined in the Week03 problems.

-- | Query Parsing | --

-- | Parses a query. A query is a complex query expression surrounded by whitespace.
parseQuery :: Parser Query
parseQuery =
  do whitespaces
     q <- parseComplexQueryExpr
     whitespaces
     return q

-- | Parses a complex query expression. A complex query expression is one of the following:
-- - A query expression that splits into a base query expression and a complex query expression, separated by some operator.
-- - A base query expression.
parseComplexQueryExpr :: Parser Query
parseComplexQueryExpr =
  do (q1, q2) <- parseDualExpr "|"
     whitespaces
     return (Pipe q1 q2)
  `orElse`
  do (q1, q2) <- parseDualExpr ","
     whitespaces
     return (Concat q1 q2)
  `orElse`
  do (q1, q2) <- parseDualExpr "=="
     whitespaces
     return (Equal q1 q2)
  `orElse`
  do (q1, q2) <- parseDualExpr "!="
     whitespaces
     return (NotEqual q1 q2)
  `orElse`
  do (q1, q2) <- parseDualExpr "<"
     whitespaces
     return (LessThan q1 q2) 
  `orElse`
  do (q1, q2) <- parseDualExpr "<="
     whitespaces
     return (LessOrEqual q1 q2) 
  `orElse`
  do (q1, q2) <- parseDualExpr ">"
     whitespaces
     return (GreaterThan q1 q2) 
  `orElse`
  do (q1, q2) <- parseDualExpr ">="
     whitespaces
     return (GreaterOrEqual q1 q2) 
  `orElse`
  do parseBaseQueryExpr

-- | Parses a base query expression. A base query expression is any query expression that is contiguous and not separated by any operators.
parseBaseQueryExpr :: Parser Query
parseBaseQueryExpr =
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
  do b <- parseBool
     whitespaces
     return (ConstBool b)
  `orElse`
  do str <- quotedString
     whitespaces
     return (ConstString str)
  `orElse`
  do num <- number
     whitespaces
     return (ConstInt num)
  `orElse`
  do failParse "Invalid query structure!"

-- | Parses a dual query expression. A dual query expression is a query expression that is split into two query expressions, separated by some operator.
parseDualExpr :: String -> Parser (Query, Query)
parseDualExpr c =
   do q1 <- parseBaseQueryExpr
      whitespaces
      stringLiteral c
      whitespaces
      q2 <- parseComplexQueryExpr
      return (q1, q2)

-- | Parses a bracketed query expression. A bracketed query expression is a query expression surrounded by brackets '(' and ')'.
parseBrackets :: Parser Query
parseBrackets =
  do isChar '('
     whitespaces
     q <- parseComplexQueryExpr
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
     q <- parseComplexQueryExpr
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
      xs <- parseComplexQueryExpr
      whitespaces
      isChar ']'
      return xs
