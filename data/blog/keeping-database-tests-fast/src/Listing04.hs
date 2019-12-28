module Listing04 where
import Database.PostgreSQL.Simple
import Database.Postgres.Temp
import Control.Exception
import Test.Hspec
import Data.Pool
import Data.IORef
import Control.Concurrent.Async
import Control.Concurrent
import Data.Foldable

abort :: (Connection -> IO a) -> Connection -> IO a
abort f conn = bracket_
  (execute_ conn "BEGIN")
  (execute_ conn "ROLLBACK") $
  f conn
