module SIGyM.Database.TH (sql) where

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Lib

-- | sql is a quasiquoter so we can write multi-line queries nicely, example:
-- 
--   allStores :: Query
--   allStores =  [sql|
--     SELECT (type, id, storeId, md5 , fs_path, fs_module, fs_symbol , db_code)
--     FROM all_stores
--     |]
-- TODO: We could parse the query to detect errors at compile time. See:
--       https://github.com/JakeWheat/hssqlppp
sql :: QuasiQuoter
sql = QuasiQuoter {
    quoteExp = litE . stringL
  , quotePat = litP . stringL
  , quoteType = let name = mkNameG_v "postgresql-simple"
                                     "Database.PostgreSQL.Simple"
                                     "Query"
                in \_ -> conT name
  }
