The simplest migration system has a single command `apply`.

`apply` connects to a database and executes a series of SQL statements. There is one interesting part of `apply`: it will not execute the same statement twice.

We call each statment the migrator can `apply` a "migration". This is the key requirement of a migration system:

> A migrator ensures migrations are only executed once.

A migrator is typically a executable with a command line interface. However good executable design treats the command line interface as a veneer over a core library. Before I talk about the user facing interface I want to talk about the migrator as a library.

However before I talk about the library, I want about migrations even more abstractly.

## Migrations Abstractly

We can consider a database to be a particular state and view migrations as function transitions the database from one state to another:

```haskell
type Migration = WorldState -> WorldState
```

`apply` has the type

```haskell
apply :: (WorldState -> WorldState) -> WorldState -> WorldState
```

or

```haskell
apply :: Migration -> Migration
```

This is similar to `$` but it has an additional property: `apply` ensures idepotency.

```haskell
apply m . apply m = apply m
```

Additionally it preserves things we would expect like `id`

```haskell
apply id = id
```

### Mo' Features Mo' Complexity

Some migrators are this simple at their core. The migrator I want to build will have a more complex interface. This is because I want to take advantage of PostgreSQL's ability to `apply` a group of migrations in a single transaction.

We can model this by imagining `apply` operates on a non-empty ordered set of `Migration`s:

```haskell
apply :: NonEmptyOrderedSet Migration -> Migration
```

The properties are similar to simplier `apply` but modified to work with the `NonEmptyOrderedSet`.

First the identity laws:

```haskell
apply {id} x = x
```

and more generally the identity migration can be absorbed in the nonempty set of migrations

```haskell
apply {x, x1, id, x2} db = apply {x, x1, x2} db
```

Pretty unexciting stuff.

The idepotent law becomes more complicated

```haskell
for s, r, t. such that s ⊕ r = t. apply s . apply r = apply t
```

So `apply`ing multiple sets of migrations is equivalent to some operation I made up `⊕`, which I'll call "overlap".

`⊕` is complicated. For now think of it as "union". This is wrong. To understand why we need to think about how migrations relate to each other.

### The Algebra of Migrations

So far we have considered `WorldState` as an atomic indivisable state of the world. However for migration systems it is useful to consider the `WorldState` as having more structure.

An ordered set of `Migration`s can be thought of a dependency graph. The separate branches commute with each other. However the parent and children of the graph do commute with each other.

A the dependencies between `Migration`s imdue a ordered set of migrations with a partial order. We can now talk about the `⊕`. `⊕` combines two sets of migrations by combining the branches and within branches append the sequence and removing duplicates. If two ordered sets have a different order for a specific branch that is a conflict and the `⊕` fails. `⊕` is a partial function.

This idea sounds complicated when phrased abstractly but work-a-day programmers understand this issue very well. If two branches have different

### Making Due

`⊕` is too hard to implement in practice. Instead we can define another operation I'll call `⊙`. It is also partial but much more restrictive. It can add to ordered sets that are either disjoint or the tail of `a` overlaps with the head of `b`.
