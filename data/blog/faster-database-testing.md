[`tmp-postgres`](http://hackage.haskell.org/package/tmp-postgres) has hit 1.0.0.0 ... more like blown past 1.0.0.0. The latest version is 1.31.0.0 as I write this.

`tmp-postgres` is a easy and fast way to make a temporary postgres database. Here is an example using [`with`](http://hackage.haskell.org/package/tmp-postgres-1.31.0.0/docs/Database-Postgres-Temp.html#v:with)

```haskell
with $ \db -> bracket
  (connectPostgreSQL (toConnectionString db))
  close $
  \conn -> execute_ conn "CREATE TABLE foo (id int)"
```

The latest version brings with it a host of improvements. It is faster, more configurable and has a better API. It is a small simple package but I have lot to say about it. Performance improvements are the most fun so let's start there.

## The Baseline

`tmp-postgres` 0.3.0.1 is last "old" version before the rewrite that led to 1.0.0.0. It's way of starting and stopping postgres is our baseline.

```
Baseline ~ 1.44 sec on macOS (0.608 sec on a Ubuntu guest with macOS host)
```

## Doing Less is Faster

`tmp-postgres` was based heavily on [`pg_tmp`](http://eradman.com/ephemeralpg/). Like `pg_tmp` it would create a `test` database using `createdb` by default. However for most purposes this is not necessary. `initdb` creates a `postgres` database we can use for testing.

```
Creating a test database ~ 1.44 (0.608) sec
No createdb step         ~ 1.11 (0.546) sec
```

## Faster Setup with `initdb` caching

Before a ephemeral postgres process can start a temporary database cluster needs to be created. This means creating a temp directory and calling `initdb` with the appriopiate arguments. This is the slowest part of naive `tmp-postgres` startup.

However for a given `initdb` version and inputs the execution is referentially transparent so we can cache the output of `initdb`. This works great in practice because the input to `initdb` rarely changes so the data cached is small.

```
No caching   ~ 1.11  (0.546) sec
With caching ~ 0.353 (0.100) sec
```

## Cow is Faster

The start up time is now mostly copying the cached cluster. Many of the files in the cluster are not modified during the duration of a test. On newer operating systems we can use "copy on write" to make the copy faster.

```
No cow ~ 0.353 (0.100) sec
cow    ~ 0.248 (0.092) sec
```

Of the remaining time it around 80% the copy and 20% shutting down postgres.

## Final Results

| | start (sec) | end (sec)| improvement |
|-|-------------|----------| ------------|
| macOS | 1.44 | 0.248 | **5.8x** |
| Ubuntu guest | 0.608 | 0.092 | **6.6x**


I've found the improved performance is large enough to be felt in real projects. I saw a 2x improvement in test time when porting [`postgresql-simple-queue`](http://hackage.haskell.org/package/postgresql-simple-queue) to the latest `tmp-postgres` version.

## But Wait, There's more!

The new `tmp-postgres` is over 5x faster on macOS and linux but the story doesn't end here. In part 2 and 3 of this blog post series I'll show how to use additionally features of `tmp-postgres` to keep database testing fast as the size of your project grows.

[Home](../index.html)
