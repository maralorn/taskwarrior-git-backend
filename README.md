# taskwarrior-git-backend
An experimental task-sync alternative via git written in Haskell

**Warning: This is experimental. The idea is that you create a git repo that contains the ground truth for your tasks. This can eat all of your tasks.**

## Usage

1. Install this app: e.g. checkout and `cabal install .` Make sure that the app is in `$PATH` after that or that you call it with absolute paths. (Make especially sure that the binary can always be found when you run taskwarrior because otherwise the hooks will fail.)
2. Create a git-repo at the path of your choice `/your/repo`.
3. Run `taskwarrior-git save -r /your/repo --commit`. This will commit all your current taskwarrior tasks to the repo.
4. Create the following on-modify and on-add hooks:

   `~/.task/hooks/on-add.git-backend`:
   ```
   taskwarrior-git on-add -r /your/repo --commit
   ```
   
   `~/.task/hooks/on-modify.git-backend`:
   ```
   taskwarrior-git on-modify -r /your/repo --commit
   ```
   This hooks will sync all changes to the git repo.
5. Configure a custom-merge tool by configuring your repo in the following way:

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
6. Run `taskwarrior-git load -r /your/repo` when ever there is a change in the git repo which was not triggered by the above hooks or init. **WARNING: This will overwrite all changes that happened in taskwarrior but where not written to the git repo.**

### Useful notes

* taskwarrior-git calls taskwarrior and therefore respects the `$TASKRC` and `$TASKDATA` environment variables.
* If you used `task sync` (which would kind of defeat the purpos of this). You would need to run `taskwarrior-git save` after every sync. Or create an on-sync hook.
* Tipp: Create a taskwarrior subcommand that pulls, rebases/merges, pushes and loads in one step. TODO: Provide an example.
* If you just never use `taskwarrior-git load` you can use this tool just for backuping your tasks.
* If you don‘t use `--commit` or `-c` this tool will actually not be very `git` specific.

## On Disk Format

Files will be saved as pretty-printed JSON without the keys `id` and `urgency` under `/your/repo/<uuid>.task`.

## Help

Run `taskwarrior-git --help`.

Do not hesitate to contact me via issue, mail or at #haskell-taskwarrior in #freenode.
