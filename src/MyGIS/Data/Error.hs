module MyGIS.Data.Error (
    EitherError
  , mapE
  , mkError
) where

import           Data.List as T (intersperse)
import           Data.Either (partitionEithers)

type Error = String
type EitherError a = Either Error a

-- | Mapea una función que devuleve 'EitherError' y devuelve o bien 'Left'
--   errores-separados-por-'sep' o 'Right' lista-de-resultados-correctos
mapE :: (a -> EitherError b) -> Error -> [a] -> EitherError [b]
mapE f sep xs = if null errors
                then Right result
                else Left . concat . intersperse sep $ errors
  where (errors,result) = partitionEithers . map f $ xs

mkError :: String -> EitherError b
mkError = Left
