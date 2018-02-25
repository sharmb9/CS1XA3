#!/bin/bash

#Checking if local repo is up to date with remote repo

#updates your local repo on the machine
git fetch
#Checks if local repo is up to date with remote 
git status

#Puts all uncommited changes in a file changes.log

git diff >> changes.log

#Puts each line from every file of your project with tag #TODO into a file todo.log

grep -r --exclude={todo.log,ProjectAnalyze.sh,changes.log,error.log} "#TODO" * >> todo.log

#Checks all haskell files for syntax errors and puts the result into error.log
find . -name "*.hs" -exec ghc -fno-code {} \; >> error.log

#Ask user if they want to view any log file
echo "Your changes, haskell errors and TODO tags have been logged into log files, do you wish to view them?"
read input 
if ["$input" == "Y"]; then 
	echo "Type C if you wish to view changes.log, type E if you wish to view errors.log and type T to view TODO.log. Type A if you wish to view your log files at once"
	read type
	if ["$type" == "C"]; then
		cat changes.log
	elif ["$type" == "E"]; then
		cat errors.log
	elif ["$type" == "T"]; then
		cat todo.log
	elif ["$type" == "A"]; then
		echo "Processing changes.log..."
		cat changes.log
		echo "Processing todo.log..."
		cat todo.log
		echo "Processing errors.log..."
		cat errors.log
	else
		echo "Oops seems like you enterd the wrong key"
		#something like break
	fi
elif ["$input" == "N"]; then
echo "Have a nice day"
#enter else statemnt for wronf key
fi

