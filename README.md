# uiuc-cs598-PSL-GroupWork
UIUC OMCS-DS Coursework: CS598 Practical Statistical Learning Group Work (Coding Assignments, Projects)

# Git Basics

At no point in time are multiple people to be working on the git repo at the same time. If 2 or all 3 group members wish to work at the same time, 1 member will be chosen as the driver and the others will assist in the background. This will help avoid most conflicts in this repository.

For the individual worker or the driver of the group work, utilize the following git workflow:

1. `git pull`: this pulls all new work into your local copy of the repository.
1. `git branch`: this will show you which branch you are working on. Ensure you are in the correct branch before starting work. To switch branches, use `git checkout branch_name`. To create a new branch, `git checkout -b new_branch_name_here`.
1. Make your code changes. 
1. Once a milestone is reached, or work has stopped for the night, add changes to your commit with `git add filename`. If you want to add all changes in your workspace to your commit, use `git add .`. You do not have to add all local changes to a commit. 
1. Commit changes with `git commit -m"commit message here"`. Commit messages should be short, but descriptive.
1. Finally, push your local changes to the use `git push`. If you are working in a new branch and pushing for the first time, use `git push --set-upstream origin new_branch_name_here`.
