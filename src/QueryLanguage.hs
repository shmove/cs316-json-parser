{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
module QueryLanguage where

import JSONTransformer
import ParserCombinators

data Query
  = Pipe        Query Query
  | Field       String
  | Elements
  | Select      Query
  | ConstInt    Int
  | ConstString String
  | Equal       Query Query
  | NotEqual    Query Query
  | LessThan    Query Query
  | LessOrEqual Query Query
  | GreaterThan Query Query
  | GreaterOrEqual Query Query
  deriving Show

-- | Executes a 'Query' by translating it into a `Transformer`. Each
-- of the constructors of 'Uery' is turned into its corresponding
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
execute (Field str)     = field str
execute Elements        = elements
execute (Select q)      = select (execute q)
execute (ConstInt n)    = int n
execute (ConstString s) = string s
execute (Equal q1 q2)          = comparison (==) (execute q1) (execute q2)
execute (NotEqual q1 q2)       = comparison (/=) (execute q1) (execute q2)
execute (LessThan q1 q2)       = comparison (<)  (execute q1) (execute q2)
execute (LessOrEqual q1 q2)    = comparison (<=) (execute q1) (execute q2)
execute (GreaterThan q1 q2)    = comparison (>)  (execute q1) (execute q2)
execute (GreaterOrEqual q1 q2) = comparison (>=) (execute q1) (execute q2)

-- HINT: this function is very similar to the 'eval' function for
-- evaluating Boolean formulas defined in the Week03 problems.

parsePipe :: Parser Query
parsePipe =
  do q1 <- parseBaseQueryExpr
     whitespaces
     isChar '|'
     whitespaces
     q2 <- parseQueryExpr
     whitespaces
     return (Pipe q1 q2)

parseComparison :: String -> Parser (Query, Query)
parseComparison c =
   do q1 <- parseBaseQueryExpr
      whitespaces
      stringLiteral c
      whitespaces
      q2 <- parseBaseQueryExpr
      whitespaces
      return (q1, q2)

parseBrackets :: Parser Query
parseBrackets =
  do isChar '('
     whitespaces
     q <- parseQueryExpr
     whitespaces
     isChar ')'
     whitespaces
     return q

-- | Parses a field name. A field name is a non-empty sequence of
-- non whitespace characters, except for the dot character '.'.
parseField :: Parser Query
parseField =
   do isChar '.'
      whitespaces
      field <- oneOrMore (satisfies "non-whitespace or chain character" (\c -> c /= ' ' && c /= '\n' && c /= '\t' && c /= '.'))
      whitespaces
      return (Field field)
      
-- | Parses chained fields. A chained field is a sequence of fields
-- separated by the dot character '.'.
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
     whitespaces
     return (Pipe field chainedField)
  `orElse`
  do parseField

parseBaseQueryExpr :: Parser Query
parseBaseQueryExpr =
  do parseBrackets
  `orElse`
  do stringLiteral "Elements"
     whitespaces
     return Elements
  `orElse`
  do stringLiteral "Select"
     whitespaces
     q <- parseQueryExpr
     return (Select q)
  `orElse`
  do parseChainedField
  `orElse`
  do str <- quotedString
     return (ConstString str)
  `orElse`
  do num <- number
     return (ConstInt num)
  `orElse`
  do failParse "Invalid query structure!"

parseQueryExpr :: Parser Query
parseQueryExpr =
  do parsePipe
  `orElse`
  do (q1, q2) <- parseComparison "=="
     return (Equal q1 q2)
  `orElse`
  do (q1, q2) <- parseComparison "!="
     return (NotEqual q1 q2)
  `orElse`
  do (q1, q2) <- parseComparison "<"
     return (LessThan q1 q2) 
  `orElse`
  do (q1, q2) <- parseComparison "<="
     return (LessOrEqual q1 q2) 
  `orElse`
  do (q1, q2) <- parseComparison ">"
     return (GreaterThan q1 q2) 
  `orElse`
  do (q1, q2) <- parseComparison ">="
     return (GreaterOrEqual q1 q2) 
  `orElse`
  do parseBaseQueryExpr
  
parseQuery :: Parser Query
parseQuery =
  do whitespaces
     q <- parseQueryExpr
     whitespaces
     return q