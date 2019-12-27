module Listing03 where
import Listing01
import Database.PostgreSQL.Simple
import Database.Postgres.Temp
import Control.Exception
import Test.Hspec
import Data.Pool
import Data.IORef
import Control.Concurrent.Async
import Control.Concurrent
import Data.Foldable

aroundAll :: forall a. ((a -> IO ()) -> IO ()) -> SpecWith a -> Spec
aroundAll withFunc specWith = do
  (var, stopper, asyncer) <- runIO $
    (,,) <$> newEmptyMVar <*> newEmptyMVar <*> newIORef Nothing
  let theStart :: IO a
      theStart = do

        thread <- async $ do
          withFunc $ \x -> do
            putMVar var x
            takeMVar stopper
          pure $ error "Don't evaluate this"

        writeIORef asyncer $ Just thread

        either pure pure =<< (wait thread `race` takeMVar var)

      theStop :: a -> IO ()
      theStop _ = do
        putMVar stopper ()
        traverse_ cancel =<< readIORef asyncer

  beforeAll theStart $ afterAll theStop $ specWith

list :: Connection -> IO [Int]
list = undefined

add :: Connection -> Int -> IO Int
add = undefined

delete :: Connection -> Int -> IO ()
delete = undefined

withPool :: (Connection -> IO a) -> Pool Connection -> IO a
withPool = flip withResource

spec :: Spec
spec = aroundAll withSetup $ describe "list/add/delete" $ do
  it "return [] initially" $ withPool $ \conn ->
    list conn `shouldReturn` []
  it "returns the created elements" $ withPool $ \conn -> do
    theId <- add conn 1
    list conn `shouldReturn` [theId]
  it "deletes what is created" $ withPool $ \conn -> do
    theId <- add conn 2
    delete conn theId
    list conn `shouldReturn` []
