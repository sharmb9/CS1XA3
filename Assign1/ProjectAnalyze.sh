#!/bin/bash

#Checking if local repo is up to date with remote repo

#updates your local repo on the machine
git fetch
#Checks if local repo is up to date with remote 
git status

#Puts all uncommited changes in a file changes.log

git diff >> changes.log

#Puts each line from every file of your project with tag #TODO into a file todo.log

grep -r --exclude="todo.log" --exclude="ProjectAnalyze.sh" "#TODO" * >> todo.log

#Checks all haskell files for syntax errors and puts the result into error.log
find . -name "*.hs" -exec ghc -fno-code {} \; >> error.log

