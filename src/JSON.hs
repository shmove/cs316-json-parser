module JSON where

-- | Representation of 'JSON' values in Haskell.
--
-- See the JSON website for more information: https://www.json.org/json-en.html
--
-- And the RFC: https://www.rfc-editor.org/rfc/rfc8259
data JSON
  = String  String -- ^ String values
  | Boolean Bool   -- ^ Boolean values
  | Null           -- ^ Null values
  | Number  Int    -- ^ Number values (here restricted to just integers)
  | Array   [JSON] -- ^ JSON Arrays, represented as a list of JSON values
  | Object  [(String, JSON)] -- ^ JSON Objects, represented as a list of field name / value pairs
  deriving (Eq, Show, Ord)

-- | Looks a up a field in a list of key/value pairs. This function is
-- only intended for use in the 'getField' function.
getFieldFromObject :: String -> [(String,JSON)] -> Maybe JSON
getFieldFromObject nm [] = Nothing
getFieldFromObject nm ((nm', json):fields) =
  if nm == nm' then Just json else getFieldFromObject nm fields

-- | Looks up a field in a JSON object. If the field does not exist,
-- or the supplied JSON is not an object, returns Nothing. Otherwise,
-- returns 'Just json' where 'json' is the JSON associated with the
-- name.
getField :: String -> JSON -> Maybe JSON
getField nm (Object fields) = getFieldFromObject nm fields
getField nm _               = Nothing

-- | Returns all the elements in a JSON array. If the supplied JSON is
-- not an array, returns 'Nothing'.
getElements :: JSON -> Maybe [JSON]
getElements (Array jsons) = Just jsons
getElements _             = Nothing

-- | Returns the boolean value of some 'JSON'. If the supplied JSON is
-- not a boolean, returns 'Nothing'.
getBool :: JSON -> Maybe Bool
getBool (Boolean b) = Just b
getBool _           = Nothing
