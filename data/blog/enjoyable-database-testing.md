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

Using `aroundAll` will speed up our testing but our test sweet is now broken. The problem is the second test leaves behind
an entry which breaks the third test.

We could make the third test more robust by getting the existing entries at the start of the test and ensure we return to the initial state, but this has other problems and in more complex real world examples it might not be clear how to make the test robust against unexpected situations, which will lead to flaky tests.

One way we can regain the isolation is by using some of the databases mechanisms for query isolation. I speak of transactions and savepoints.

To faciliate this here is an example `rollback` function:

```haskell
-- TODO I guess this also wraps things in a transaction
rollback :: (Connection -> IO a) -> Connection -> IO a
rollback = undefined
```

We can now prefix our tests with rollback and they will not interfer with each other

```haskell
aroundAll withSetup $ describe "list/add/delete" $ do
  it "return [] initially" $ withPool $ rollback $ \conn ->
    list conn `shouldReturn` []
  it "returns the created elements" $ withPool $ rollback $ \conn ->
    theId <- add conn 1
    list conn `shouldReturn` [theId]
  it "deletes what is created" $ withPool $ rollback $ \conn ->
    theId <- add conn 2
    delete conn theId
    list conn `shouldReturn` []
```

It is worth pointing out that not every sql statement can be run in a transaction. Additionally statements like `TRUNCATE` cannot be rolled back. That said the fast majority of queries are meant to be run in a transaction and `rollback` will be sufficent to return the database to a clean state.

## The Pleasure and Pain of `parallel`

Database queries are meant to be run in parallel with other queries. We can take advantage of this by running our tests in parallel to improve performance.

We can change our test to run in parallel by adding the `parallel` combinator:

```haskell
aroundAll withSetup $ describe "list/add/delete" $ parallel $ do
  it "return [] initially" $ withPool $ rollback $ \conn ->
    list conn `shouldReturn` []
  it "returns the created elements" $ withPool $ rollback $ \conn ->
    theId <- add conn 1
    list conn `shouldReturn` [theId]
  it "deletes what is created" $ withPool $ rollback $ \conn ->
    theId <- add conn 2
    delete conn theId
    list conn `shouldReturn` []
```

Unfortunately if we run our tests again we will notice failures.

The problem is our `rollback` function is wrapping everything in a `READ_COMMITTED` isolation level. This is equivalent to each statement receiving it's own snapshot of the database but we need the entire transaction to have a single consistent snapshot of the database so we need to use either `REPEATABLE_READ` or `SERIALIZABLE` isolation.

In our particular example the solution is to use the higher isolation and retry on serializable errors. However this is not always possible.

For instance some postgres statements do not work well in more consistent isolation levels because they provide an intrisitically inconsistent picture of the database (the main example I know of is `SKIP LOCKED` but there might be others).

This is the case with `postgresql-simple-queue` however I was still able utilize `parallel` to improve performance by starting separate `postgres` instances.

The startup cost of `tmp-postgres` is around 250 ms on Mac or 90 ms on Linux so your tests will need to be over around 2 seconds for this approach to be helpful.

Here is what it would look like in our example ... although admittantly not necessary:

```haskell
describe "list/add/delete" $ parallel $ do
  aroundAll withSetup $ do
    it "return [] initially" $ withPool $ rollback $ \conn ->
      list conn `shouldReturn` []
    it "returns the created elements" $ withPool $ rollback $ \conn ->
      theId <- add conn 1
      list conn `shouldReturn` [theId]

  aroundAll withSetup $ do
    it "deletes what is created" $ withPool $ rollback $ \conn ->
      theId <- add conn 2
      delete conn theId
      list conn `shouldReturn` []
```

Starting a separate `postgres` instance is a big hammer. It is a heavy weight operation but can surprisingly help performance in some situations and provides the highest level of isolation in tests which can improve reliability.

# Recap

When testing with `tmp-postgres` using `cacheAction`, `rollback` and separate `postgres` instances can help keep test suites fast even as the projects grow larger.

In the next blog post in the series I'll show how to use `tmp-postgres` to diagnosis and fix performance problems in the queries under test.
