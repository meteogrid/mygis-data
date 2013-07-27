module SIGyM.Database.Pool (
    ConnectionPool
  , ConnectInfo (..)

  , defaultConnectInfo

  , createConnectionPool
  , withPoolTransaction
) where 

import           Database.PostgreSQL.Simple ( Connection, ConnectInfo(..), connect, close
                                            , withTransaction )
import           Data.Pool (Pool, createPool, withResource)
import           Data.Time.Clock (NominalDiffTime)


type ConnectionPool = Pool Connection

defaultConnectInfo :: ConnectInfo
-- | Default connection options. Override as needed like:
-- >>> myInfo = defaultConnectInfo { connectUser = "foo" }
defaultConnectInfo = ConnectInfo {
    connectHost     = "" -- use unix domain socket
  , connectPort     = 5432
  , connectUser     = "sigym"
  , connectPassword = ""
  , connectDatabase = "sigym"
  }

createConnectionPool ::
     ConnectInfo
  -> Int
  -> NominalDiffTime
  -> Int
  -> IO ConnectionPool
-- | Creates a PostgreSQL Connection pool
createConnectionPool info numStripes idleTime maxConnections =
  createPool (connect info) close numStripes idleTime maxConnections

withPoolTransaction :: ConnectionPool -> (Connection -> IO a) -> IO a
-- | Executes an action with a Connection inside a transaction
withPoolTransaction pool act  = withResource pool $ \c -> withTransaction c (act c)
