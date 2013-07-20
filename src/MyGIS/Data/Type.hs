{-# LANGUAGE DeriveDataTypeable #-}
module MyGIS.Data.Type (
    Type
  , mkType
) where

import           Data.Data (Typeable, Data)
import           Data.Text (Text)
import           Data.String (IsString, fromString)

newtype Type = Type Text deriving (Eq, Show, Typeable, Data)

mkType :: String -> Type
mkType s = Type (fromString s)
