# taskwarrior-git-backend
An experimental task-sync alternative via git written in Haskell

**Warning: This is experimental. The idea is that you create a git repo that contains the ground truth for your tasks. This can eat all of your tasks.**

**Work-in-progress: The features below are not yet implemented**

## Usage

1. Install this app: e.g. checkout and `cabal install .` Make sure that the app is in `$PATH` after that or that you call it with absolute paths. (Make especially sure that the binary can always be found when you run taskwarrior because otherwise the hooks will fail.)
2. Create a git-repo at the path of your choice `/your/repo`.
3. Run `taskwarrior-git save -r /your/repo`. This will commit all your current taskwarrior tasks to the repo.
4. Create the following on-modify and on-add hooks:
```
> cat ~/.task/hooks/on-add.git-backend
taskwarrior-git on-add -r /your/repo
> cat ~/.task/hooks/on-modify.git-backend
taskwarrior-git on-modify -r /your/repo
```
This hooks will sync all changes to the git repo.
5. Configure a custom-merge tool by adding this to your `.git/config` in your repo:
```
taskwarrior-git merge
```
6. Run `taskwarrior-git load -r /your/repo` when ever there is a change in the git repo which was not triggered by the above hooks or init. **WARNING: This will overwrite all changes that happened in taskwarrior but where not written to the git repo.**

Hint: taskwarrior-git calls taskwarrior and therefor respects the TASKRC and TASKDATA environment variables.

Note: If you used `task sync` (which would kind of defeat the purpos of this). You would need to run `taskwarrior-git save` after every sync. Or create an on-sync hook.

Tipp: Create a taskwarrior subcommand that pulls, rebases, pushes and loads in one step.
