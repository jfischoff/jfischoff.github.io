# `pg-transact`

# benchmarks
- The lack a `with` resource api for criterion is annoying
- `abort` is twice as fast as `rollback`

  ```
  Running 1 benchmarks...
  Benchmark benchmark: RUNNING...
  benchmarking rollback
  time                 114.1 μs   (112.2 μs .. 117.1 μs)
                       0.998 R²   (0.997 R² .. 1.000 R²)
  mean                 113.0 μs   (112.4 μs .. 113.9 μs)
  std dev              2.546 μs   (1.524 μs .. 4.065 μs)
  variance introduced by outliers: 18% (moderately inflated)

  benchmarking abort
  time                 53.08 μs   (52.89 μs .. 53.35 μs)
                       1.000 R²   (1.000 R² .. 1.000 R²)
  mean                 53.15 μs   (52.97 μs .. 53.56 μs)
  std dev              850.7 ns   (486.4 ns .. 1.343 μs)
  variance introduced by outliers: 11% (moderately inflated)
  ```

  but they are both fast I can't see the difference mattering.

# 0.3.1.1
- No reason to have a abort take in an action. You can't nest them. This is clearer if the type is `abort :: DBT m a`.
- Okay well I guess there is a reason. Throwing an abort is different then saying `finally` abort. So I think the current form is useful.
- However I think what is missing is a `ROLLBACK` in the runner.
- Scratch that. The runner already rollbacks on exceptions. I don't need the extra rollback in `abort` call. Test pass. Ship it.
