module Listing05 where
import Listing01
import Listing03
import Listing04
import Database.PostgreSQL.Simple
import Database.Postgres.Temp
import Control.Exception
import Test.Hspec
import Data.Pool
import Data.IORef
import Control.Concurrent.Async
import Control.Concurrent
import Data.Foldable

spec :: Spec
spec = aroundAll withSetup $ describe "list/add/delete" $ do
  it "return [] initially" $ withPool $ abort $ \conn ->
    list conn `shouldReturn` []
  it "returns the created elements" $ withPool $ abort $ \conn -> do
    theId <- add conn 1
    list conn `shouldReturn` [theId]
  it "deletes what is created" $ withPool $ abort $ \conn -> do
    theId <- add conn 2
    delete conn theId
    list conn `shouldReturn` []
