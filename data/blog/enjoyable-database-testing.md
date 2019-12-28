You've written some Haskell code and it compiles ... so it works?

Hahahahahahahaha ha ... sigh ... *[muffled sobbing]*.

Turns out this is not the case. Not the case in general and definitely not the case if you are writing database queries.

You can try to find various libraries to limit what needs to be tested but at some point you are going to want to test your database code.

`tmp-postgres` can help you write reliable tests. There are a lot of ways you could utilize `tmp-postgres` to write your tests but I'll show you what I find works best for me.

The blog post also shows general database tests good practices that are not tied to using `tmp-postgres` or `postgresql-simple`.

## Starting `postgres` Quickly

We need to create a fast `tmp-postgres` setup function.

First will utilize `initdb` caching by using [`withDbCache`](https://hackage.haskell.org/package/tmp-postgres-1.34.0.0/docs/Database-Postgres-Temp.html#v:withDbCache). As I discussed [previously](/faster-database-testing.html)
this gives a 3-4x performance boost. However in "real" projects the overhead in database testing tends to come from the time it takes to create a migrated database. Running a complete set of database migration can easily take 10 seconds.
To speed up this process we need a way to cache a database cluster after the migrations have run.

`tmp-postgres` provides the [`cacheAction`](https://hackage.haskell.org/package/tmp-postgres-1.34.0.0/docs/Database-Postgres-Temp.html#v:cacheAction) function to cache migrated database clusters. Here is the type signature:

```haskell
cacheAction :: FilePath -> (DB -> IO ()) -> Config -> IO (Either StartError Config)
```

If database cluster folder (the first argument) does not exist, the continuation (second argument) will run. The third argument is to configure the temporary database which makes the cluster. `cacheAction` returns a Config that can be used to start a database initialized with the cached database cluster referred to in the first argument.

Long story short you should use `cacheAction` to store a database cluster at the state after the migration has been run stored at a location based on the hash of the migration query and (any other previous hashes ... if you wanted to cache every migration for instance). If you do this you won't have to run your migrations every time you run your tests.

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

Let's recap what this function does:
- Create a persistent `initdb` cache with `withDbCache`.
- Caches the `migration` action by storing a premigrated database cluster at the folder given by `"~/.tmp-postgres/" <> hash`.
- Starts a postgres instance with the migrated database cluster.
- Creates a pool of database connections for tests to use for connecting to the ephemeral database.

For these examples I'm going to assume that `postgresql-simple` is the database
library the queries are written in. However the same techniques could be used for other database libraries.

We can now use this function to provide a `Pool Connection` for our tests with the some resource helpers.

```haskell
around withSetup $ describe "list/add/delete" $ do
  it "return [] initially" $ withPool $ \conn -> do
    list conn `shouldReturn` []
  it "returns the created elements" $ withPool $ \conn -> do
    theId <- add conn 1
    list conn `shouldReturn` [theId]
  it "deletes what is created" $ withPool $ \conn -> do
    theId <- add conn 2
    delete conn theId
    list conn `shouldReturn` []
```

## Minimize the Creation of Database Clusters

The example above is a valid way to test queries but it is unlikely to be the optimal way.

The problem is [`around`](http://hackage.haskell.org/package/hspec-2.7.1/docs/Test-Hspec.html#v:around) creates a isolated postgres cluster for every test. Even with all of our fancy caching
this is still pretty time consuming compared to our queries which will be in the low single digit millisecond range.

To limit the overhead starting a ephemeral database it is best to create the minimal number of database clusters.

`hspec` is missing an `aroundAll` function. Here is one that I use: [aroundAll](https://gist.github.com/jfischoff/5cf62b82e1dd7d03c8a610ef7fd933ff)

We can use it to replace the `around` in our previous example:

```haskell
aroundAll withSetup $ describe "list/add/delete" $ do
  it "return [] initially" $ withPool $ \conn -> do
    list conn `shouldReturn` []
  it "returns the created elements" $ withPool $ \conn -> do
    theId <- add conn 1
    list conn `shouldReturn` [theId]
  it "deletes what is created" $ withPool $ \conn -> do
    theId <- add conn 2
    delete conn theId
    list conn `shouldReturn` []
```

## The Rub

Using `aroundAll` will speed up our testing but our test suite is now broken. The problem is the second test leaves behind
an entry breaking the third test.

We could make the third test more robust by caching the state at the start of the test and ensuring we return to the initial state:

```haskell
  it "deletes what is created" $ withPool $ \conn -> do
    before <- list conn

    theId <- add conn 2
    delete conn theId
    list conn `shouldReturn` before
```

There are still potential problems with this modification.

At the end of the day it might not be clear how to make the test robust.

One way we can regain the isolation of separate database clusters and `postgres` instances is by using the databases mechanisms for query isolation.

To faciliate query isolation we'll write an function to wrap a list of statements in a transaction that we rollback instead of committing:

```haskell
abort :: (Connection -> IO a) -> Connection -> IO a
abort f conn = bracket_
  (execute_ conn "BEGIN")
  (execute_ conn "ROLLBACK")
  (f conn)
```

We can now prefix our tests with abort and they will not interfer with each other

```haskell
aroundAll withSetup $ describe "list/add/delete" $ do
  it "return [] initially" $ withPool $ abort $ \conn -> do
    list conn `shouldReturn` []
  it "returns the created elements" $ withPool $ abort $ \conn -> do
    theId <- add conn 1
    list conn `shouldReturn` [theId]
  it "deletes what is created" $ withPool $ abort $ \conn -> do
    theId <- add conn 2
    delete conn theId
    list conn `shouldReturn` []
```

It is worth pointing out that not every PostgreSQL sql statement can run in a transaction. Additionally there is no isolation level that can bring the database to the same state the way starting at a cluster snapshot can. Things like series are incremented and not decrememented on rollback among other MVCC infidelities. That said the fast majority of queries are perfectly isolated using  transaction and `abort` will be sufficent to return the database to a original state (for all intensive purposes).

## The Pleasure and Pain of `parallel`

Database queries are meant to be run in parallel with other queries. We can take advantage of this by running our tests in parallel to improve performance.

We can change our test to run in parallel by adding the `parallel` combinator:

```haskell
aroundAll withSetup $ describe "list/add/delete" $ parallel $ do
  it "return [] initially" $ withPool $ abort $ \conn -> do
    list conn `shouldReturn` []
  it "returns the created elements" $ withPool $ abort $ \conn -> do
    theId <- add conn 1
    list conn `shouldReturn` [theId]
  it "deletes what is created" $ withPool $ abort $ \conn -> do
    theId <- add conn 2
    delete conn theId
    list conn `shouldReturn` []
```

Unfortunately if we run our tests again we will notice failures.

The problem is our `abort` function is wrapping everything in a `READ_COMMITTED` isolation level. This is equivalent to each statement receiving it's own snapshot of the database but we need the entire transaction to have a single consistent snapshot of the database so we need to use either `REPEATABLE_READ` or `SERIALIZABLE` isolation.

In our particular example the solution is to use the higher isolation and retry on serializable errors. However this is not always possible.

For instance some postgres statements, like `SKIP LOCKED`, do not run performantly in more consistent isolation levels because they provide an intrisitically inconsistent picture of the database.

This made using `SERIALIZABLE` unusable in testing `postgresql-simple-queue` however I was still able utilize `parallel` to improve performance, while maintaining isolation through separate `postgres` instances.

The startup cost of `tmp-postgres` is around 250 ms on Mac or 90 ms on Linux so your tests will need to be atleast 0.6 seconds but probabaly closer to 2.0 seconds for this approach to be meaningfully helpful.

Here is what it would look like in our example:

```haskell
describe "list/add/delete" $ parallel $ do
  aroundAll withSetup $ do
    it "return [] initially" $ withPool $ abort $ \conn -> do
      list conn `shouldReturn` []
    it "returns the created elements" $ withPool $ abort $ \conn -> do
      theId <- add conn 1
      list conn `shouldReturn` [theId]

  aroundAll withSetup $ do
    it "deletes what is created" $ withPool $ abort $ \conn -> do
      theId <- add conn 2
      delete conn theId
      list conn `shouldReturn` []
```

Starting a separate `postgres` instance is a big hammer. It is a heavyweight operation but can surprisingly help performance in some situations and provides the highest level of isolation. However typically `abort` is optimal.

## Reuse setup with `rollback`

`abort` is great for rolling back all the changes of test but sometimes we would like to only rollback some changes in a test.

This situation arises when we would like to reuse some setup to run a few different assertions.

```haskell
it "returns [] after all removal operations" $ withPool $ abort $ \conn -> do
  complexSetup conn
  forM_ allRemovalOperations $ \op -> rollback $ op conn >> list `shouldReturn` []
```

Unlike `abort` `rollback` can be nested and does not abort the entire transaction.

We can implement `rollback` with savepoints like:

```haskell
rollback :: Connection -> IO a -> IO a
rollback conn actionToRollback = mask $ \restore -> do
  sp <- savepoint conn
  restore actionToRollback `finally` rollbackToAndReleaseSavepoint conn sp
```

Since `rollback` uses savepoints the performance is not as good  as `abort` which is twice as fast. However they are both sub-millisecond operations so I am not sure choosing one or the other matters.

## Clean Up

Our tests are fast. That's important. Slow tests die.

We can now make them prettier.

### Using a Connection Monad

These tests are fast but they are kinda of ugly because all of the `conn` threading. We can use a `ReaderT Connection IO` monad or something morally similar to it to implicitly pass the `conn` `Connection` parameter (yes ... we could use `ImplicitParams`).

Here is our cleaned up example:

```haskell
describe "list/add/delete" $ parallel $ do
  aroundAll withSetup $ do
    it "return [] initially" $ withPool $ abort $ do
      list `shouldReturn` []
    it "returns the created elements" $ withPool $ abort $ do
      theId <- add 1
      list `shouldReturn` [theId]

  aroundAll withSetup $ do
    it "deletes what is created" $ withPool $ abort $ do
      theId <- add 2
      delete theId
      list `shouldReturn` []
```

### Fold `abort` and `withPool` into `it`

```haskell
describe "list/add/delete" $ parallel $ do
  let wit msg = it msg . withPool . abort

  aroundAll withSetup $ do
    wit "return [] initially" $ do
      list `shouldReturn` []
    wit "returns the created elements" $ do
      theId <- add 1
      list `shouldReturn` [theId]

  aroundAll withSetup $ do
    wit "deletes what is created" $ do
      theId <- add 2
      delete theId
      list `shouldReturn` []
```

Alright good enough.

# Recap

When testing with `tmp-postgres` using `cacheAction`, `abort`, `rollback` and separate `postgres` instances can help keep test suites fast even as the projects grow larger. Additionally a connection monad or similar can make the tests look cleaner.

In the next blog post in the series I'll show how to use `tmp-postgres` to diagnosis and fix performance problems in the queries under test.

A major pain of database testing I have not addressed is how to build test data that has foreign key references. I'll have to come back to this after showing off `tmp-postgres`.
