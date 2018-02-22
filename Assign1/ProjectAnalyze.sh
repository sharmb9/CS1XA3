#!/bin/bash

#Checking the status of the repo

git status

#Puts all uncommited changes in a file changes.log

git diff > changes.log

#Puts each line from every file of your project with tag #TODO into a file todo.log

grep -r --exclude="todo.log" "#TODO" * > todo.log

