# taskwarrior-git-backend
An experimental task-sync alternative via git written in Haskell

**Warning: This is experimental. The idea is that you create a git repo that contains the ground truth for your tasks. This can eat all of your tasks.**

## Usage

1. Install this app: e.g. checkout and `cabal install .` Make sure that the app is in `$PATH` after that or that you call it with absolute paths. (Make especially sure that the binary can always be found when you run taskwarrior because otherwise the hooks will fail.)
2. Create a git-repo at the path of your choice `/your/repo`.
3. Create a config file, either at `~/.config/taskwarrior-git/config.dhall` or at `$TASKWARRIOR_GIT_CONFIG`. (Later takes precedence if exists.
The config format is [Dhall](https://dhall-lang.org).
{
  repository = "/your/repo", -- This Option is mandatory.
  commit = True -- Default is False
}
4. Run `taskwarrior-git save`. This will commit all your current taskwarrior tasks to the repo.
5. Create the following on-modify and on-add hooks:

   `~/.task/hooks/on-add.git-backend`:
   ```
   taskwarrior-git on-add
   ```

   `~/.task/hooks/on-modify.git-backend`:
   ```
   taskwarrior-git on-modify
   ```
   This hooks will sync all changes to the git repo.
6. Configure a custom-merge tool by configuring your repo in the following way:

   `/your/repo/.gitattributes`:
   ```
   *.task merge=tasks
   ```

   `/your/repo/.git/config`:
   ```
   ...

   [merge "tasks"]
     name = task merge driver
     driver = taskwarrior-git merge %O %A %B
   ```
   If you jump this step you can still use this app. But you might get merge-conflicts.
7. Run `taskwarrior-git load` when ever there is a change in the git repo which was not triggered by the above hooks or init. **WARNING: This will overwrite all changes that happened in taskwarrior but where not written to the git repo.**

### Useful notes

* taskwarrior-git calls taskwarrior and therefore respects the `$TASKRC` and `$TASKDATA` environment variables.
* If you used `task sync` (which would kind of defeat the purpos of this). You would need to run `taskwarrior-git save` after every sync. Or create an on-sync hook.
* Tipp: Create a taskwarrior subcommand that pulls, rebases/merges, pushes and loads in one step. TODO: Provide an example.
* If you just never use `taskwarrior-git load` you can use this tool just for backuping your tasks.
* If you let `commit = False` this tool will actually not be very `git` specific.

## On Disk Format

Files will be saved as pretty-printed JSON without the keys `id` and `urgency` under `/your/repo/<uuid>.task`.

## Pros & Cons

### Pros

* You get all the advantages from git. An understandable history and you probably know exactly how to setup sync.

### Cons

* This breaks modified dates a bit. Taskwarrior ignores imported modified timestamps and sets them based on the time of import. This will in general also mean, that in the history of changes to one task changes that happend on another host will have the timestamp of the import and not the original timestamp. (Taskwarrior git will not commit a task as changed if only the modified date changed to avoid a lot of unnecessary commits.)

### Merge algorithm

* I don‘t know exactly how the `task sync` merge algorithm works. My guess is that my merge algorithm might be better in some situations and worse in others. But better is probably subjective here anyways.

## Help

Run `taskwarrior-git --help`.

Do not hesitate to contact me via issue, mail or at #haskell-taskwarrior in #freenode.
