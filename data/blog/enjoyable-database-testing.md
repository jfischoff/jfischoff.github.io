You've written some Haskell code and it compiles ... so it works?

Hahahahahahahaha ha ... sigh ... *[muffled sobbing]*.

Turns out this is not the case. Not the case in general and definitely not the case if you are writing database queries.

You can try to find various libraries to limit what needs to be tested but at some point you are going to want to test your database code.

`tmp-postgres` can help you write reliable tests. There are a lot of ways you could utilize `tmp-postgres` to write your tests but I'll show you what I find works best for me.


## Starting `postgres` Quickly

We need to create a fast `tmp-postgres` setup function.

First will utilize `initdb` caching by using `withDbCache`. As I discussed [previously](/faster-database-testing.html)
this gives a 3-4x performance boost. However in "real" projects the overhead in database testing tends to come from the time it takes to create a migrated database.
mature proje
Running a complete set of database migration can easily take 10 seconds.
To speed up this process we need a way to cache a database cluster after the migrations have run.

To faciliate this `tmp-postgres` provides the [`cacheAction`](https://hackage.haskell.org/package/tmp-postgres-1.34.0.0/docs/Database-Postgres-Temp.html#v:cacheAction) function. Here is the type signature:

```haskell
cacheAction :: FilePath -> (DB -> IO ()) -> Config -> IO (Either StartError Config)
```

If database cluster folder (the first argument) does not exist, the continuation (second argument) will run. The third argument is to configure the temporary database which makes the cluster. `cacheAction` returns a Config that can be used to start a database initialize with the cached database cluster referred to in the first argument.

Long story short you should use `cacheAction` to store a database cluster at the state after the migration has been run stored at a location based on the hash of the migration query. If you do this you won't have to run your migrations every time you run your tests.

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
    theId <- add conn 1
    list conn `shouldReturn` [theId]
  it "deletes what is created" $ withPool $ \conn ->
    theId <- add conn 2
    delete conn theId
    list conn `shouldReturn` []
```

## Minimize the Creation of Database Clusters

The example above is a valid way to test queries but it is unlikely to be the optimal way.

The problem is [`around`](http://hackage.haskell.org/package/hspec-2.7.1/docs/Test-Hspec.html#v:around) creates a isolated postgres cluster for every test. Even with all of our fancy caching
this is still pretty time consuming compared to our queries which will be in the low single digit millisecond range.

To limit the overhead starting a database, incures it is best to create the minimal number of database clusters.

`hspec` is missing an `aroundAll` function. Here is one that I use: [aroundAll](https://gist.github.com/jfischoff/5cf62b82e1dd7d03c8a610ef7fd933ff)

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

Using `aroundAll` will speed up our testing but our test suite is now broken. The problem is the second test leaves behind
an entry breaking the third test.

We could make the third test more robust by caching the state at the start of the test and ensuring we return to the initial state, but this has other problems and in more complex real world examples it might not be clear how to make the test robust against unexpected situations, which will lead to flaky tests.

One way we can regain the isolation is by using some of the databases mechanisms for query isolation. I speak of transactions and savepoints.

To faciliate this here is an example `abort` function:

```haskell
abort :: (Connection -> IO a) -> Connection -> IO a
abort f conn = query_ conn "BEGIN" >> f conn >> query_ conn "ROLLBACK"
```

We can now prefix our tests with abort and they will not interfer with each other

```haskell
aroundAll withSetup $ describe "list/add/delete" $ do
  it "return [] initially" $ withPool $ abort $ \conn ->
    list conn `shouldReturn` []
  it "returns the created elements" $ withPool $ abort $ \conn ->
    theId <- add conn 1
    list conn `shouldReturn` [theId]
  it "deletes what is created" $ withPool $ abort $ \conn ->
    theId <- add conn 2
    delete conn theId
    list conn `shouldReturn` []
```

It is worth pointing out that not every sql statement can be run in a transaction. Additionally statements like `TRUNCATE` cannot be rolled back. That said the fast majority of queries are meant to be run in a transaction and `abort` will be sufficent to return the database to a clean state.

## The Pleasure and Pain of `parallel`

Database queries are meant to be run in parallel with other queries. We can take advantage of this by running our tests in parallel to improve performance.

We can change our test to run in parallel by adding the `parallel` combinator:

```haskell
aroundAll withSetup $ describe "list/add/delete" $ parallel $ do
  it "return [] initially" $ withPool $ abort $ \conn ->
    list conn `shouldReturn` []
  it "returns the created elements" $ withPool $ abort $ \conn ->
    theId <- add conn 1
    list conn `shouldReturn` [theId]
  it "deletes what is created" $ withPool $ abort $ \conn ->
    theId <- add conn 2
    delete conn theId
    list conn `shouldReturn` []
```

Unfortunately if we run our tests again we will notice failures.

The problem is our `abort` function is wrapping everything in a `READ_COMMITTED` isolation level. This is equivalent to each statement receiving it's own snapshot of the database but we need the entire transaction to have a single consistent snapshot of the database so we need to use either `REPEATABLE_READ` or `SERIALIZABLE` isolation.

In our particular example the solution is to use the higher isolation and retry on serializable errors. However this is not always possible.

For instance some postgres statements do not work well in more consistent isolation levels because they provide an intrisitically inconsistent picture of the database (the main example I know of is `SKIP LOCKED` but there might be others).

This is the case with `postgresql-simple-queue` however I was still able utilize `parallel` to improve performance by starting separate `postgres` instances.

The startup cost of `tmp-postgres` is around 250 ms on Mac or 90 ms on Linux so your tests will need to be over around 0.5 seconds for this approach to be helpful.

Here is what it would look like in our example ... although admittantly not necessary:

```haskell
describe "list/add/delete" $ parallel $ do
  aroundAll withSetup $ do
    it "return [] initially" $ withPool $ abort $ \conn ->
      list conn `shouldReturn` []
    it "returns the created elements" $ withPool $ abort $ \conn ->
      theId <- add conn 1
      list conn `shouldReturn` [theId]

  aroundAll withSetup $ do
    it "deletes what is created" $ withPool $ abort $ \conn ->
      theId <- add conn 2
      delete conn theId
      list conn `shouldReturn` []
```

Starting a separate `postgres` instance is a big hammer. It is a heavy weight operation but can surprisingly help performance in some situations and provides the highest level of isolation in tests.


## Reuse setup with `rollback`

`abort` is great for rollback all the changes of test but sometimes we would like to only rollback some changes in a test.

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

Since `rollback` uses savepoints the performance is not as good as rolling back the entire transaction for `abort` still has value.

## Clean Up

Our tests are fast. That's important. Slow tests die.

We can now make them prettier.

### Using a Connection Monad

This tests are fast but they are kinda of ugly because all of the `conn` threading. We can use a `ReaderT Connection IO` monad or something morally similar to it to implicitly pass the `conn` `Connection` parameter.

Here is our cleaned up example:

```haskell
describe "list/add/delete" $ parallel $ do
  aroundAll withSetup $ do
    it "return [] initially" $ withPool $ abort $
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

### Fold `abort` into `withPool`

```haskell
describe "list/add/delete" $ parallel $ do
  aroundAll withSetup $ do
    it "return [] initially" $ withPool $
      list `shouldReturn` []
    it "returns the created elements" $ withPool $ do
      theId <- add 1
      list `shouldReturn` [theId]

  aroundAll withSetup $ do
    it "deletes what is created" $ withPool $ do
      theId <- add 2
      delete theId
      list `shouldReturn` []
```

I use [`pg-transact`](https://hackage.haskell.org/package/pg-transact) for this but you could use any transaction monad like [`postgresql-transactional`](https://hackage.haskell.org/package/postgresql-transactional).

# Recap

When testing with `tmp-postgres` using `cacheAction`, `abort`, `rollback` and separate `postgres` instances can help keep test suites fast even as the projects grow larger. Additionally a connection monad can make the tests look cleaner.

In the next blog post in the series I'll show how to use `tmp-postgres` to diagnosis and fix performance problems in the queries under test.

A major pain of database testing I have not addressed is how to build test data that has foreign key references when you don't care about the closure or connected relations. I'll have to come back to this after showing off `tmp-postgres`.
