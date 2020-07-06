Recently I was working on moving `hasql-queue` to partitioned tables. The performance degraded and I realized the plan was different.

The new partition table version used a `Bitmap Index Scan` while the old version used `Index Scan`. Also the new plans estimated row count was `35` while the old faster plan had an estimated row count of `1`.

I knew the `Bitmap Index Scan` was the problem but what about partitions would cause it to get used?

I tried to channel my coworker Travis Staton. How would he reason about this?

The `Bitmap Index Scan` occurs when the estimated row count is higher than a typical `Index Scan`. This means the index selectivity was lower.

Then I remembered I had changed the primary key to be a compound key of two columns `(id, modified)`. This meant when looking up the first value alone, `id`, there might be duplicate values.

This is different from a look up with a single column primary key. There can be only one row returned in that case.

The long story short is if you see a `Bitmap Index Scan` and you think it is unnecessary, see if you have compound index. You might need to give the postgres planner an index with sufficent uniqueness.

[Home](../index.html)
