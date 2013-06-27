module MyGIS.Data.Error (
    EitherError
  , mapE
) where

import           Data.Text as T (Text, intercalate)
import           Data.Either (partitionEithers)

type Error = Text
type EitherError a = Either Error a

-- | Mapea una función que devuleve 'EitherError' y devuelve o bien 'Left'
--   errores-separados-por-'sep' o 'Right' lista-de-resultados-correctos
mapE :: (a -> EitherError b) -> Error -> [a] -> EitherError [b]
mapE f sep xs = if null errors
                then Right result
                else Left . T.intercalate sep $ errors
  where (errors,result) = partitionEithers . map f $ xs
