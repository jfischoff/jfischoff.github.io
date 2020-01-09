# [`trek`](https://github.com/jfischoff/trek)
- I am not sure if `inputGroup` will work for cmd line interface.
- The `InputMigration` is probably a query string and some meta info. However the `InputGroup` is a directory path. Creating an `InputGroup` from a `InputMigration`s is an IO operation so I need the signature to be `inputGroup :: NonEmpty InputMigration -> DB InputGroup`
- I don't see where `inputGroup` is used outside of the tests so it should probably be moved out of the interface as well.
- I can remove the `Version` from the interface.
- I am not a fan of the `inputAction`. I'm not a fan of having a separate interface so I can test things.
- I am surprised I can't make a single interface that can be used for building the libraries and the tests.
- I'm not sure testing an extended interface means you have tested the interface.

- I am starting to think I need to modify the TestInterface.hsig.
  - `clear` seems unnecessary. `rollback` should be enough.

- I think I am going to simplify the interface and then see if I can rewrite the tests without the test interface.

- Including the `inputGroup` in the interface was a way to ensure that `InputGroup` was non-empty ... but it can't do that anyway.
- There is nothing about the interface that points to it not handling an empty inputgroup. That is not something that core checks.
- I think what is needed for testing is `toOutput :: InputMigration -> OutputRecord` but I can't in general compute that. For instance if a rollback PiTR label is generated, or if anything is generated in the DB. I think preventing that from happening is too draconian.
- ~~The way the current tests must work is using `inputVersion   :: InputMigration -> Version` and `list` to verify versions are added currently but nothing else.~~ No `list` and `apply` both return `OutputGroup`s

- I think the properties for just apply are:
  - Empty migrations give Nothing

- Removing some notes from `InterfaceSpec.hs` to here for prosperity.

      the migrate filePath -> dispatches based type of file
      if it is a directory it tries to load each file
      -- it dispatches of the file type
      if it is a file it loads it as a newline manifest
      if it is a sql file it runs it and using the name to determine the migration
      if it is a sh it runs it

      Crazy idea. I can test the migrate filePath implementations with the same test interface

      Crazy idea more the exe can shelf test the extension scripts

      trek add-hashes [VERSION]

      trek remove-hashes [VERSION]

      Some ideas

      need to make the job runner next

      data Job = Job
        { batch :: LastRow -> DB [Row]
        , rowKey :: Row -> LastRow
        , action :: Row -> DB ()
        }

      And the coordinator that can take a migration and turn it into a sequence of
      migrations and jobs followed by code deployments.

      These might all be migrators

      The job system is sort of like a migration system that has a status
      The job system updates the migration until it is finished.

      The code deployment doesn't push something that is already out there

      Every thing is idepotent. It is basically a system for ensure idepotency

      but they are idepotent in different ways.
      The migration system does something or not.
      The job is incremental and updates state
      The code deployment is based on the output hash.

      Not clear how to have a single interface yet ... if at all.

      an interesting question is whether the system can handle concurrency. It should
      be able to

      A table lock sounds reasonable

      Some thoughts about the bigger picture. This is not just a migrator.
      It is a way to store actions to help achieve idempotency. The fact
      that it can do that easily for DB actions is a special case. Because
      we can lift into the DB (or perhaps it should be abstracted to a different
      monad) we can embed arbitrary IO.

      In this way the migrator can orchanstrate the steps to a zero down time
      deployment.

- One of the things I am noticing. The value of the current `list/apply` tests is I did not need to know how to convert an inputgroup to an outputgroup.
