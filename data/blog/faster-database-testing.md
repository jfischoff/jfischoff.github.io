*updated 12/23/19*

[`tmp-postgres`](http://hackage.haskell.org/package/tmp-postgres) has hit 1.0.0.0 ... more like blown past 1.0.0.0. The latest version is 1.34.0.0 as I write this.

`tmp-postgres` is an easy and fast way to make a temporary postgres database. Here is an example using [`with`](http://hackage.haskell.org/package/tmp-postgres-1.31.0.0/docs/Database-Postgres-Temp.html#v:with):

```haskell
with $ \db -> bracket
  (connectPostgreSQL (toConnectionString db))
  close $
  \conn -> execute_ conn "CREATE TABLE foo (id int)"
```

The latest version brings with it a host of improvements. It is faster, more configurable and has a better API. It is a small, simple package but I have a lot to say about it. Performance improvements are the most fun, so let's start there.

## Doing Less is Faster

`tmp-postgres` 0.3.0.1 is the last "old" version before the rewrite that led to 1.0.0.0. Its way of starting and stopping postgres is our baseline.

`tmp-postgres` was based heavily on [`pg_tmp`](http://eradman.com/ephemeralpg/). Like `pg_tmp` it created a `test` database using `createdb` by default. However for most purposes this is not necessary. `initdb` creates a `postgres` database we can use for testing.

||Baseline (sec) |No `createdb` step (sec) |
|-|-|-|
macOS | 1.44 | 1.11|
Ubuntu guest| 0.608 | 0.546|

## Faster Setup with `initdb` caching

Before an ephemeral postgres process can start a temporary database a cluster needs to be created. This means creating a temp directory and calling `initdb` with the appropriate arguments. This is the slowest part of a naive `tmp-postgres` startup.

However for a given `initdb` version and inputs the execution is referentially transparent so we can cache the output of `initdb`. This works great in practice because the input to `initdb` rarely changes so the data cached is small.

||No caching (sec)|With `initdb` caching (sec)|
|-|-|-|
macOS | 1.11 | 0.353 |
Ubuntu guest| 0.546 | 0.100 |

## COW is Faster

The start up time is now mostly copying the cached cluster. Many of the files in the cluster are not modified during the duration of a test. On newer operating systems we can use "copy on write" to make the copy faster.

||No COW (sec) | COW (sec) |
|-|-|-|
macOS | 0.353 | 0.248 |
Ubuntu guest| 0.100 | 0.092 |

Of the remaining time, around 80% is the copy and 20% shutting down postgres.

## Final Results

| | Start (sec) | End (sec)| Improvement |
|-|-------------|----------| ------------|
| macOS | 1.44 | 0.248 | **5.8x** |
| Ubuntu guest | 0.608 | 0.092 | **6.6x**


I've found the improved performance is large enough to be felt in real projects. I saw a 2x improvement in test time when porting [`postgresql-simple-queue`](http://hackage.haskell.org/package/postgresql-simple-queue) to the latest `tmp-postgres` version.

## But Wait, There's More!

The new `tmp-postgres` is over 5x faster on macOS and linux but the story doesn't end here. In parts 2 and 3 of this blog post series I'll show how to use additional features of `tmp-postgres` to keep database testing fast as the size of your project grows.

[Home](../index.html)
