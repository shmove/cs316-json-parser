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
execute (Equal q1 q2)   = equal (execute q1) (execute q2)

-- HINT: this function is very similar to the 'eval' function for
-- evaluating Boolean formulas defined in the Week03 problems.

parsePipe :: Parser Query
parsePipe =
  do q1 <- parseBaseQueryExpr
     whitespaces
     isChar '|'
     whitespaces
     q2 <- parseQueryExpr
     return (Pipe q1 q2)

parseEqual :: Parser Query
parseEqual =
  do q1 <- parseBaseQueryExpr
     whitespaces
     isChar '='
     whitespaces
     q2 <- parseQueryExpr
     return (Equal q1 q2)

parseElements :: Parser ()
parseElements =
  do stringLiteral "Elements"
     whitespaces
     return ()

parseSelect :: Parser Query
parseSelect =
  do stringLiteral "Select"
     whitespaces
     q <- parseQueryExpr
     return (Select q)

parseBaseQueryExpr :: Parser Query
parseBaseQueryExpr =
  do parseElements
     return Elements
  `orElse`
  do select <- parseSelect
     return select
  `orElse`
  do field <- identifier
     return (Field field)
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
  do parseEqual
  `orElse`
  do parseBaseQueryExpr
  
parseQuery :: Parser Query
parseQuery =
  do whitespaces
     q <- parseQueryExpr
     whitespaces
     return q