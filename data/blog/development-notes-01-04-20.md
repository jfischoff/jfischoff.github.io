# [`trek`](https://github.com/jfischoff/trek)
- Here is my current way I would describe this project.
  - There is a core library that is built around the functions: `setup`, `teardown`, `apply` and `list`.
  - The MVP command line will have `apply` and `create`.
  - The core library doesn't have to a way to parse queries currently but I might need a way to create a query from a
    filepath: `FilePath -> IO (DB ())` (or should it be `FilePath -> IO (DB a)`?).
  - We need to make an autosetup interface for just `apply`. The properties that the `setup` necessary `apply` must     statisfy if the db has been `setup` are the properties that auto-setup `apply` must have. The `setup` necessary     `apply` must pass a superset of the auto-setup `apply`'s properties. I'm not being very clear.
  - I'm not sure if I will ever expose the more complex interface or not. But it is also an implementation for the
    auto-setup interface.
  - That is all the core autosetup apply interface must specify. And there must be a way to adapt the `apply :: IO ()`
    to reuse the tests.
  - If a core test fails against the executable we can rerun on different levels to find where it is breaking.

- Things I need to do
  - Make the autosetup `apply` only hsig (and project!).
  - Make the `apply` with `setup` adapter.
  - Refactor the `apply` properties to the autosetup Spec and have the with `setup` Spec depend on it.
  - Make the new cmdline hsig.
  - Make the `apply :: IO ()` adapter for the core properties.
  - Finish the tests for cmd line interface.
