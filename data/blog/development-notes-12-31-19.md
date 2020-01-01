# [`trek`](https://github.com/jfischoff/trek)

- CI is now compiling as much as I am locally. So time to get local compiling. I think I might have quit working on this when I hit a point where `with` would be useful in `tmp-postgres`.
- Scratch that getting an error from `PartialOptions` locally that do not get in CI.
- `postgresql-simple-opts >= 0.5.0.1` is busted. I was kinda of hoping I would not have to fix that right away. This is the problem and value of dog fooding I guess.
- Hopingfully I can modify `trek` to not depend on `postgresql-simple-opts >= 0.5.0.1`.
- I still can't believe I named the package "opts" instead of "options". I spelled out "postgresql-simple" but "options" was too much ... smh.
- I might be able to fix it by adjusting the dependencies.
- `therewebe` is using `postgresql-simple-opts >= 0.5.0.1`. I must of already fixed it and bumped the version.
- I fixed it for `postgres-options-0.1.0.1`. It is broken against 0.2.0.0. I just need to fix it.
- The readme I am copying around could be very wrong.
- Fixed the `postgresql-simple-opts` version issues. Now onto a `tmp-postgres` error.
- Commenting out Database.Trek.ToInterfaceImpl.hashConflicts because I don't remember what I was
  doing.
- The whole code is built around `SpecState` that is a db runner and shutdown function. Instead I should
- use `aroundAll` and the whole thing should just need a "with" interface of `(forall a. DB a -> IO a)`.

# [`postgresql-simple-opts`](https://github.com/jfischoff/postgresql-simple-opts)

- I should get this working the latest stackage nightly and then readd it to stackage.
- The new version of `postgres-options` is really a partial options. The `PartialOptions` is unnecessary.
  I'm leaving it now but I'm probably going to remove it.
- On the otherhand I haven't revisited the decision to make the `Option` type have so
  many optional fields. It seems fine. This could just have orphans..idk...or stay the way it is.
