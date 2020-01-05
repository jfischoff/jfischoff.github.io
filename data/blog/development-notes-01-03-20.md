# [`trek`](https://github.com/jfischoff/trek)
- I question whether I can use the core tests for the MVP interface.
- The self setup `apply` has fewer tests cases. I think it is a subset of the `apply` with `setup`.
- Every `apply` with `setup` can be used to instaniate a `apply` with auto-setup interface.
- I don't like the `CmdLine.hsig`. For one thing it assumes it gets a list of command line arguments but no
  environment variables. It just seems unnecessary.
- I think `apply :: IO ()` is probably the right interface. I can hopefully catch the `ExitFailure`s.
- I will need to capture its standard handles when testing.
- `apply :: IO ()` with be built with an autosetup db `apply`.
- So it `apply :: IO ()`:
  - read the environment variables and make a data connection.
  - read the migration from a file. catch and error and exit.
  - parse the migration file name. catch and error and exit.
  - run the autosetup apply. catch and error and exit.
  - print result
- I should fix this interface and get things to compile.
