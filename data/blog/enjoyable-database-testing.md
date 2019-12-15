You've written some Haskell code and it compiles ... so it works?

Hahahahahahahaha ha ... sigh ... *muffled sobbing*.

Turns out this is not the case. Not the case in general and definitely not the case if you are writing database queries.

You can try to find various libraries to limit what needs to be tested but at some point you are going to want to test your database code.

`tmp-postgres` can help you write reliable tests. There are a lot of ways you could utilize `tmp-postgres` to write your tests but I'll show you what I find works best for me.


## Starting `postgres` Quickly

We need to create a fast `tmp-postgres` setup function.

First will utilize `initdb` caching by using `withDbCache`. As I discussed [previously](/faster-database-testing.html)
this gives a 3-4x performance boost. However in "real" projects the overhead in database testing tends to come from the time it takes to create a migrated database.

Running a complete set of database migration can easily take 10 seconds on mature project.
To speed up this process we need a way to cache a database cluster after the migrations have been run.

To faciliate this `tmp-postgres` provides the [`cacheAction`](https://hackage.haskell.org/package/tmp-postgres-1.31.0.1/docs/Database-Postgres-Temp.html#v:cacheAction) function. Here is the type signature:

```haskell
cacheAction :: FilePath -> (DB -> IO ()) -> Config -> IO (Either StartError Config)
```

`cacheAction` will conditionally start a database cluster using the provided `Config` (third argument), if folder specified the first argument does not exist. After starting the database it will call the second argument and pass the `DB` handle. It will then shutdown the database and cache the database cluster to the location specified by the first argument.

Finally it will unconditionally return a `Config` that uses the database cluster at the location specified by the first argument.

Long story short you should hash your migrations and use them for cache path and use a migration action for the second argument.

Here is an example `tmp-postgres` setup function that does all the right things:

```haskell
withSetup :: (Pool Connection -> IO ()) -> IO ()
withSetup f = do
  -- Helper to throw exceptions
  let throwE x = either throwIO pure =<< x

  throwE $ withDbCache $ \dbCache -> do
    let combinedConfig = defaultConfig <> cacheConfig dbCache
    migratedConfig <- throwE $ cacheAction ("~/.tmp-postgres/" <> hash) migrate combinedConfig
    withConfig migratedConfig $ \db ->
      f =<< createPool (connectPostgreSQL $ toConnectionString db) close 2 60 10
```

Let's recap what this function does
- Create a persistent `initdb` cache with `withDbCache`.
- Caches the `migration` action by storing a premigrated database cluster at `"~/.tmp-postgres/" <> hash`.
- Starts a postgres instance with the migrated database cluster.
- Creates a pool of database connections for tests to use for connecting to temporary database.

For these examples I'm going to assume that `postgresql-simple` is the database
library the queries are written in. However the same techniques could be used for other database libraries.

We can now use this function to provide a `Pool Connection` for our tests with the some resource helpers.

```haskell
around withSetup $ describe "list/add/delete" $ do
  it "return [] initially" $ withPool $ \conn ->
    list conn `shouldReturn` []
  it "returns the created elements" $ withPool $ \conn ->
    theId <- add 1
    list conn `shouldReturn` [theId]
  it "deletes what is created" $ withPool $ \conn ->
    theId <- add 2
    delete theId
    list conn `shouldReturn` []
```

## Minimize the Creation of Database Clusters

The example above is a valid way to test queries but it is unlikely to be the optimal way.

The problem is `around` creates a isolated postgres cluster for every test. Even with all of our fancy caching
this is still pretty time consuming compared to our queries which will be in the low single digit millisecond range.

To limit the overhead starting a database, incures it is best to create the minimal number of database clusters.

`hspec` is missing an `aroundAll` function. Here is one that I use:

```haskell
aroundAll :: forall a. ((a -> IO ()) -> IO ()) -> SpecWith a -> Spec
aroundAll withFunc specWith = do
  (var, stopper, asyncer) <- runIO $
    (,,) <$> newEmptyMVar <*> newEmptyMVar <*> newIORef Nothing
  let theStart :: IO a
      theStart = do
        thread <- async $ withFunc $ \x -> do
          putMVar var x
          takeMVar stopper

        writeIORef asyncer $ Just thread

        takeMVar var

      theStop :: a -> IO ()
      theStop _ = do
        putMVar stopper ()
        traverse_ wait =<< readIORef asyncer

  beforeAll theStart $ afterAll theStop $ specWith
```

We can use it to replace the `around` in our previous example:

```haskell
aroundAll withSetup $ describe "list/add/delete" $ do
  it "return [] initially" $ withPool $ \conn ->
    list conn `shouldReturn` []
  it "returns the created elements" $ withPool $ \conn ->
    theId <- add conn 1
    list conn `shouldReturn` [theId]
  it "deletes what is created" $ withPool $ \conn ->
    theId <- add conn 2
    delete conn theId
    list conn `shouldReturn` []
```

## The Rub

Using `aroundAll` will speed up our testing but it our test sweet is now broken. The problem is the second test leaves behind
an entry which breaks the third test.

We could make the third test more robust by getting the existing entries at the start of the test and ensure we return to the initial state, but this has other problems and in more complex real world examples it might not be clear how to make the test robust against unexpected situations, which will lead to flaky tests.

One way we can regain the isolation is by using some of the databases mechanisms for query isolation. I speak of transactions and savepoints.


I typically start with a single temporary database and add more if it makes sense (will cover the reasons you might want to do this later in the post).

```haskell
withConn :: Temp.DB -> (Connection -> IO a) -> IO a
withConn db f =
  bracket (connectPostgreSQL $ toConnectionString db) close f
```

```
withSetup :: (Connection -> IO ()) -> IO ()
withSetup f = either throwIO pure <=< withDbCache $ \dbCache -> do
  migratedConfig <- either throwIO pure =<<
      cacheAction
        migrationHash
        (flip withConn (migrate schemaName))
        (defaultConfig <> cacheConfig dbCache)
  withConfig migratedConfig $ withConn
```
