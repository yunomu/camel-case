module Case where

import Data.Char (isUpper, toUpper, toLower)

toCamelCase :: String -> String
toCamelCase [] = []
toCamelCase ('_':c:cs) = toUpper c:toCamelCase cs
toCamelCase (c:cs) = toLower c:toCamelCase cs

data Case = UpperCase | LowerCase
  deriving (Enum, Eq, Ord, Read, Show)

toSnakeCase :: Case -> String -> String
toSnakeCase _ [] = []
toSnakeCase cap (c:cs)
    | isUpper c = '_':f c:toSnakeCase cap cs
    | otherwise = f c:toSnakeCase cap cs
  where
    f = case cap of
        UpperCase -> toUpper
        LowerCase -> toLower
