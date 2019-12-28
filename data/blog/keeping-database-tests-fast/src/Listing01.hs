module Listing01 where
import Data.Pool
import Database.PostgreSQL.Simple
import Database.Postgres.Temp
import Control.Exception

hash :: String
hash = undefined

migrate :: DB -> IO ()
migrate = undefined

withSetup :: (Pool Connection -> IO ()) -> IO ()
withSetup f = do
  -- Helper to throw exceptions
  let throwE x = either throwIO pure =<< x

  throwE $ withDbCache $ \dbCache -> do
    let combinedConfig = defaultConfig <> cacheConfig dbCache
    migratedConfig <- throwE $ cacheAction ("~/.tmp-postgres/" <> hash) migrate combinedConfig
    withConfig migratedConfig $ \db ->
      f =<< createPool (connectPostgreSQL $ toConnectionString db) close 2 60 10
