# `therewebe`

- Started to look at why the insert time is so high for the first insert when running the tests (14ms vs 0.3ms).
- `pg_prewarm` seemed to help but there were still buffers that were loaded. I wasn't sure what relations were being loaded so
  I just dumped all the buffer pages before and after (using `pg_buffercache`). Turns out there are bunch of system pages that are loaded for the first insert. I'm not really sure if it is worth trying to address that.

- I think the reason that postgres does not attribute the time the query takes to the query (the plan and timing are different) is that much of the time is from loading these system pages to cache. That's my theory at least.
