# [`trek`](https://github.com/jfischoff/trek)
- If I change the interface of the "core" library to based on a String query I can't reuse it for some of the
  of the other ideas I have (jobs, etc)
- However I am not trying to do those other things now, so I am not sure it worth paying a complexity cost.
- Whether I use `String` in `InputMigration` or not, I need have way to use the interface that in involves a creating a migration from a string.
- I think there are two different concerns.
  - I want to be able to write tests for the migration interface that do not have to change if the implementation
    changes
  - I want to be able to have a interface for building a migrator that let's the implementation change.

  For testing I care less about the specifics of creation. For instance the implementation might have a name or not
  but that doesn't change the roundtrip property of the migrations.

  However whether it has a name of not changing what is need for creation so it is an important part of the interface for
  migrator.

  My current feeling is it is better to have two interfaces that perfectly fit the needs to how they are being used, then one but we'll see.

  I think this puts the question of whether to use a `DB ()` or a `String` in more focus. From the perspective of the `apply`
  property tests it doesn't matter.

  Choosing one or the another doesn't solve the problem that I would like to have a separate interface for actually making the migrator.

- I think for now there is an interface for building the migrator that requires a String. However I can still implement it with a `DB ()`.
- I think the question is whether I want to write a postgresql function that is used as the implementation for apply. I do want an adhoc way to add migrations with postgresql functions but the interface is different. It will only take in a single migration and derive the hash and version. I don't need a function for adding an array of migration inputs.
- So I think on final consideration nothing is gained really by moving to a `String` version.
- I can make the `applyMigrations` function faster with a `executeMany` but I don't think I should worry about that.
