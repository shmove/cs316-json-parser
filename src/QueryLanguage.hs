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
  | Add            Query Query
  | Subtract       Query Query
  | Multiply       Query Query
  | Divide         Query Query
  | Modulo         Query Query
  | None
  deriving (Show, Eq)

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
execute (Add q1 q2)            = tAdd (execute q1) (execute q2)
execute (Subtract q1 q2)       = tSub (execute q1) (execute q2)
execute (Multiply q1 q2)       = tMul (execute q1) (execute q2)
execute (Divide q1 q2)         = tDiv (execute q1) (execute q2)
execute (Modulo q1 q2)         = tMod (execute q1) (execute q2)
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

-- | Parses a combinatory query expression. A combinatory query expression is one of the following:
-- - A query expression that splits into a base query expression and a combinatory query expression, separated by some combinatory operator.
-- - A conditional query expression.
parseCombinatoryQueryExpr :: Parser Query
parseCombinatoryQueryExpr =
  do (q1, q2) <- combinatoryExpr "|"
     whitespaces
     return (Pipe q1 q2)
  `orElse`
  do (q1, q2) <- combinatoryExpr ","
     whitespaces
     return (Concat q1 q2)
  `orElse`
  parseConditionalQueryExpr

-- | Parses a conditional query expression. A conditional query expression is one of the following:
-- - A query expression that splits into a base query expression and a conditional query expression, separated by some conditional operator.
-- - A comparison query expression.
parseConditionalQueryExpr :: Parser Query
parseConditionalQueryExpr =
  do (q1, q2) <- conditionalExpr "and"
     whitespaces
     return (And q1 q2)
  `orElse`
  do (q1, q2) <- conditionalExpr "&&"
     whitespaces
     return (And q1 q2)
  `orElse`
  do (q1, q2) <- conditionalExpr "or"
     whitespaces
     return (Or q1 q2)
  `orElse`
  do (q1, q2) <- conditionalExpr "||"
     whitespaces
     return (Or q1 q2)
  `orElse`
   parseComparisonQueryExpr

-- | Parses a comparison query expression. A comparison query expression is one of the following:
-- - A query expression that splits into a comparison query expression and a conditional query expression,
--   separated by some comparison operator.
-- - An arithmetic query expression.
parseComparisonQueryExpr :: Parser Query
parseComparisonQueryExpr =
  do (q1, q2) <- comparisonExpr "=="
     whitespaces
     return (Equal q1 q2)
  `orElse`
  do (q1, q2) <- comparisonExpr "!="
     whitespaces
     return (NotEqual q1 q2)
  `orElse`
  do (q1, q2) <- comparisonExpr "<"
     whitespaces
     return (LessThan q1 q2) 
  `orElse`
  do (q1, q2) <- comparisonExpr "<="
     whitespaces
     return (LessOrEqual q1 q2) 
  `orElse`
  do (q1, q2) <- comparisonExpr ">"
     whitespaces
     return (GreaterThan q1 q2) 
  `orElse`
  do (q1, q2) <- comparisonExpr ">="
     whitespaces
     return (GreaterOrEqual q1 q2)
  `orElse`
   parseArithmeticQueryExpr

-- | Parses an arithmetic query expression. An arithmetic query expression is one of the following:
-- - A query expression that forms an arithmetic expression built from base query expressions and arithmetic operators.
-- - A base query expression.
parseArithmeticQueryExpr :: Parser Query
parseArithmeticQueryExpr =
  do q <- arithmeticExpr
     whitespaces
     return q

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
  -- do q <- parseArrayItems
  --    whitespaces
  --    return q
  -- `orElse`
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
combinatoryExpr :: String -> Parser (Query, Query)
combinatoryExpr c =
  do q1 <- parseConditionalQueryExpr
     whitespaces
     stringLiteral c
     whitespaces
     q2 <- parseCombinatoryQueryExpr
     return (q1, q2)

-- 
conditionalExpr :: String -> Parser (Query, Query)
conditionalExpr c =
   do q1 <- parseComparisonQueryExpr
      whitespaces
      stringLiteral c
      whitespaces
      q2 <- parseConditionalQueryExpr
      return (q1, q2)

-- | Parses a comparison expression. A comparison expression is an expression that is split into
-- a base query expression and a comparison query expression, separated by some operator.
comparisonExpr :: String -> Parser (Query, Query)
comparisonExpr c =
   do q1 <- parseBaseQueryExpr
      whitespaces
      stringLiteral c
      whitespaces
      q2 <- parseComparisonQueryExpr
      return (q1, q2)

-- | Parses an arithmetic expression. An arithmetic expression is an expression that is split into
-- a base query expression and another arithmetic expression, separated by some arithmetic operator.
arithmeticExpr :: Parser Query
arithmeticExpr = do subtractExpr

-- | Parses a subtract expression. A subtract expression is an expression that is split into
-- an add expression and another subtract expression, separated by the '-' operator.
subtractExpr :: Parser Query
subtractExpr =
  do e1 <- addExpr
     whitespaces
     isChar '-'
     whitespaces
     e2 <- subtractExpr
     return (Subtract e1 e2)
  `orElse`
   addExpr

-- | Parses an add expression. An add expression is an expression that is split into
-- a multiply expression and another add expression, separated by the '+' operator.
addExpr :: Parser Query
addExpr =
  do e1 <- multiplyExpr
     whitespaces
     isChar '+'
     whitespaces
     e2 <- addExpr
     return (Add e1 e2)
   `orElse`
   multiplyExpr

-- | Parses a multiply expression. A multiply expression is an expression that is split into
-- a modulo expression and another multiply expression, separated by the '*' operator.
multiplyExpr :: Parser Query
multiplyExpr =
  do e1 <- moduloExpr
     whitespaces
     isChar '*'
     whitespaces
     e2 <- multiplyExpr
     return (Multiply e1 e2)
   `orElse`
   moduloExpr

-- | Parses a modulo expression. A modulo expression is an expression that is split into
-- a divide expression and another modulo expression, separated by the '%' operator.
moduloExpr :: Parser Query
moduloExpr =
  do e1 <- divideExpr
     whitespaces
     isChar '%'
     whitespaces
     e2 <- moduloExpr
     return (Modulo e1 e2)
   `orElse`
   divideExpr

-- | Parses a divide expression. A divide expression is an expression that is split into
-- a base query expression and another divide expression, separated by the '/' operator.
-- If any expression is not an arithmetic expression, then it is parsed as a base query expression.
divideExpr :: Parser Query
divideExpr =
  do e1 <- parseBaseQueryExpr
     whitespaces
     isChar '/'
     whitespaces
     e2 <- divideExpr
     return (Divide e1 e2)
   `orElse`
   parseBaseQueryExpr

-- | Parses a bracketed query expression. A bracketed query expression is any query expression surrounded by brackets '(' and ')'.
parseBrackets :: Parser Query
parseBrackets =
  do isChar '('
     q <- parseQuery
     isChar ')'
     return q

-- | Parses a select expression. A select expression is the keyword 'select' followed by a bracketed query expression.
parseSelect :: Parser Query
parseSelect =
  do stringLiteral "select"
     whitespaces
     q <- parseBrackets
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
-- A field name can be given in one of the following ways:
-- - A quoted string.
-- - A sequence of non-whitespace / disallowed characters.
-- - A quoted string surrounded by square brackets '[' and ']'.
--
-- A field name can also be the identity operator '.'.
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
      isChar '['
      whitespaces
      fstr <- quotedString
      isChar ']'
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

-- Removed for poor functionality
-- -- | Parses an array of queries. An array of queries is a sequence of query expressions separated by commas ',' and surrounded by square brackets '[' and ']'.
-- parseArrayItems :: Parser Query
-- parseArrayItems =
--    do isChar '['
--       xs <- parseQuery
--       isChar ']'
--      return xs
