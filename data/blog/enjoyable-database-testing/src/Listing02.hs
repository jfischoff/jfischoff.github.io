module Listing02 where
import Listing01
import Database.PostgreSQL.Simple
import Database.Postgres.Temp
import Control.Exception
import Test.Hspec
import Data.Pool

list :: Connection -> IO [Int]
list = undefined

add :: Connection -> Int -> IO Int
add = undefined

delete :: Connection -> Int -> IO ()
delete = undefined

withPool :: (Connection -> IO a) -> Pool Connection -> IO a
withPool = flip withResource

spec :: Spec
spec = around withSetup $ describe "list/add/delete" $ do
  it "return [] initially" $ withPool $ \conn ->
    list conn `shouldReturn` []
  it "returns the created elements" $ withPool $ \conn -> do
    theId <- add conn 1
    list conn `shouldReturn` [theId]
  it "deletes what is created" $ withPool $ \conn -> do
    theId <- add conn 2
    delete conn theId
    list conn `shouldReturn` []
