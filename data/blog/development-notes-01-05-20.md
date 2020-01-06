# [`trek`](https://github.com/jfischoff/trek)
- I've decided that the extra work of writing adapters to maintain the more complex db core is not the fastest path. I should remove the functionality and only maintain a db core that has the interface the cmd line interface supports.
- However before I start the process of removing the features, I should get what I currently have to compile in some sense.
- Trying to get CI working.
- My problem with private libraries and --dep is affecting someone else: https://github.com/haskell/cabal/issues/6081
- I'm trying to call --dep on the packages in a order that might work ... idk
  - cabal v2-build --enable-tests --dep trek-db-interface
  - cabal v2-build --enable-tests --dep trek
  - cabal v2-build --dep trek-db-spec (I am now wondering if I can install the deps for without the tests first)
  - cabal v2-build --dep all (this is working now ... not sure if it would work from a clean. Need to check.)
  - Even after install the non test deps I can't install the test deps.
  - I think I am going to have to manually include test deps in the non-test parts.
- Remove CI cache and pray.
- The `trek-db-spec` has a reference implementation that is pure that is used to test the spec. I wanted to do the same thing with `trek-cmd-line-spec` but I don't think I can make a pure implementation because it needs to connect to the db. That is part of what I am testing I think. That it can parse the environment variables and connect.
- I think I could make a reference implementation using IORefs that doesn't connect to a the db. I don't see all the pieces right now and I am not sure if it is worth it.
- I need to list out the tests for `apply` and `create`
- If I am going to test that the hashes are something specific I need to specify the way the application `hash` is
  calculated. I could just concat the hashes and hash the result. However then I would have an identical hash for the application and migration which would make them not globally unique. However if I append "application|" to the hash I could maintain global uniqueness probably. I'm going to do the former but I have to document it either way.
