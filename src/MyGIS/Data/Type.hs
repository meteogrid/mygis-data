module MyGIS.Data.Type (
    Type
  , mkType
) where

import           Data.Text (Text)
import           Data.String (IsString, fromString)

newtype Type = Type Text deriving (Eq, Show)

mkType :: String -> Type
mkType s = Type (fromString s)
