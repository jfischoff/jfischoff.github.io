# Introducing `hasql-queue`

I've recently `hasql-queue`, a PostgreSQL backed queue for the `hasql` ecosystem.

`hasql-queue` began as a port of `postgresql-simple-queue` but has evolved a new interface and many performance improvements.

# An Overview

`hasql-queue` has two modes of operations: high throughput and low throughput.

## The High Throughput API

The high throughput API has the folowing API

```haskell
enqueue :: E.Value a -> [a] -> Session ()

dequeue :: D.Value a -> Int -> Session [a]

dequeued :: D.Value a -> Maybe PayloadId -> Int -> Session (PayloadId, [a])
```

Essentially the API has an enqueue, dequeue and dequeued list functions. Both `enqueue` and `dequeue` can operate on batches of elements. Additionally as is customary with `hasql` one must pass in the `Value a` for encoding and decoding the payloads. This is performance improvement from the `postgresql-simple-queue` which used `jsonb` to store all payloads.

`dequeued` Takes in a optional `PayloadId` which is an opaque identifier for efficient "key-set" paginations of dequeued elements.

There is not a lot here and that is on purpose. The API is simple so let's look under the surface.

### Schema

For the API to be useable a compatible schema must exist in the database. To help create the necessary tables the `Hasql.Queue.Migrate` is provided.

Here is the schema it creates:

```sql
  CREATE TYPE state_t AS ENUM ('enqueued', 'dequeued', 'failed');

  CREATE SEQUENCE IF NOT EXISTS modified_index START 1;

  CREATE TABLE IF NOT EXISTS payloads
  ( id BIGSERIAL PRIMARY KEY
  , attempts int NOT NULL DEFAULT 0
  , state state_t NOT NULL DEFAULT 'enqueued'
  , modified_at int8 NOT NULL DEFAULT nextval('modified_index')
  , value ${valueType} NOT NULL
  ) WITH (fillfactor = 50);

  CREATE INDEX IF NOT EXISTS active_modified_at_idx ON payloads USING btree (modified_at)
    WHERE (state = 'enqueued');
```

Notice the `valueType` that must be passed in and match the `Value a` that is used in the API above. For instance for `Value Text` a `text` type should be used for `valueType`.

For the high throughput API only the states `enqueued` and `dequeued` are used (We will come back to `failed`).

`postgresql-simple-queue` used to use a `timestamptz` to determine the oldest element to dequeue. However the performance of timestamps are much slower to generate sequeuences of `int8`.

Like `postgresql-simple-queue`, `hasql-queue` uses a partial index to track the `enqueued` elements.

This is the minimal recommended schema. The actual `payloads` table one uses could have more columns in it but it needs these columns and related DDL statements at a minimum.

### `enque`

Under the hood `enque` has seperate SQL statements for enqueuing a batch and enqeuing a single element. The sql for enqeueing multiple elements is:

```sql
INSERT INTO payloads (attempts, value)
SELECT 0, * FROM unnest($1)
```

where the argument `$1` is a an array of elements. However for a single element the following is used because it is faster:

```sql
INSERT INTO payloads (attempts, value)
VALUES (0, $1)
```

### `dequeue`

`dequeue` has the following SQL for batches:

```sql
UPDATE payloads
SET state='dequeued'
WHERE id in
  ( SELECT p1.id
    FROM payloads AS p1
    WHERE p1.state='enqueued'
    ORDER BY p1.modified_at ASC
    FOR UPDATE SKIP LOCKED
    LIMIT $1
  )
RETURNING value
```

This is not the most straightforward way to write `dequeue`. However this is a fast way to write `dequeue`. It utilizes `UPDATE SKIP LOCKED` a PostgreSQL feature added to support work queues like `hasql-queue`.

`UPDATE SKIP LOCKED` takes row level locks, but as the name suggests, skips rows that are already locked by other sessions.

While the locks are held we update the state to `dequeued` and return the elements.

The sql for the single element version is identical but instead of `$1` uses `1`. PostgreSQL is able to reuse plans for stored procedures that take in zero arguments, so inlining the `1` and using a specialized version is slightly faster.

### Ideal operation

TODO explain that ideally batches should be used but this is not always possible.

# Benchmarks

TODO show benchmarks for different numbers of concurrent producers and consumers

# Low throughput API

When the throughput of enqueuing is high it is most efficent to continously poll the db for more data. However if the throughput is low, polling is wasteful.
