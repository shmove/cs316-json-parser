module JSONInput where

import JSON
import Result
import ParserCombinators

-- This is the JSON parser from the Week08 notes, adjusted to work
-- with the slightly different parser combinator API in
-- ParserCombinators.


parseJSONBool :: Parser Bool
parseJSONBool =
  do stringLiteral "true"
     return True
  `orElse`
  do stringLiteral "false"
     return False

parseNull :: Parser ()
parseNull =
  do stringLiteral "null"
     return ()

comma :: Parser ()
comma = isChar ','

spacedComma :: Parser ()
spacedComma =
  do whitespaces
     isChar ','
     whitespaces

parseList :: Parser a -> Parser [a]
parseList p =
  do isChar '['
     whitespaces
     xs <- sepBy spacedComma p
     whitespaces
     isChar ']'
     return xs

parseObjectItem :: Parser a -> Parser (String, a)
parseObjectItem p =
  do fieldname <- quotedString
     whitespaces
     isChar ':'
     whitespaces
     value <- p
     return (fieldname, value)

parseObject :: Parser a -> Parser [(String,a)]
parseObject p =
  do isChar '{'
     whitespaces
     xs <- sepBy spacedComma (parseObjectItem p)
     whitespaces
     isChar '}'
     return xs

parseJSON :: Parser JSON
parseJSON =
  do num <- number
     return (Number num)
  `orElse`
  do s <- quotedString
     return (String s)
  `orElse`
  do b <- parseJSONBool
     return (Boolean b)
  `orElse`
  do parseNull
     return Null
  `orElse`
  do items <- parseList parseJSON
     return (Array items)
  `orElse`
  do fields <- parseObject parseJSON
     return (Object fields)
  `orElse`
  failParse "Couldn't parse JSON"

stringToJSON :: String -> Result JSON
stringToJSON = completeParse (do json <- parseJSON
                                 whitespaces
                                 return json)
