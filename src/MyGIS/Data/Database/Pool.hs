module MyGIS.Data.Database.Pool (
    PGConnectionPool
  , ConnectionOptions

  , createConnectionPool
  , createPGConnectionPool
  , withPoolTransaction
) where 


import           Database.HDBC (IConnection (..), withTransaction)
import           Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import           Data.Pool (Pool, createPool, withResource)
import           Data.Time.Clock (NominalDiffTime)


type PGConnectionPool = Pool Connection
type ConnectionOptions = [(String,String)]

-- | Creates a IConnection pool
createConnectionPool :: IConnection conn
  => (String -> IO conn)
  -> String
  -> Int
  -> NominalDiffTime
  -> Int
  -> IO (Pool conn)
createConnectionPool connect uri numStripes idleTime maxConnections =
  createPool (connect uri) disconnect numStripes idleTime maxConnections

-- | Creates a PostgreSQL Connection pool
createPGConnectionPool ::
  ConnectionOptions -> Int -> NominalDiffTime -> Int -> IO PGConnectionPool
createPGConnectionPool = createConnectionPool connectPostgreSQL . toDBUri

-- | Executes an action with a HDBC IConnection inside a transaction
withPoolTransaction :: IConnection conn => Pool conn -> (conn -> IO a) -> IO a
withPoolTransaction pool = withResource pool . (flip withTransaction)

toDBUri :: ConnectionOptions -> String
toDBUri = unwords . map (\(k, v) -> k ++ "=" ++ v)
