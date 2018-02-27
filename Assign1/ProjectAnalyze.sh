#!/bin/bash

#Checking if local repo is up to date with remote repo
function checkStatus()
{
#updates your local repo on the machine
git fetch
#Checks if local repo is up to date with remote
echo $(git status)
}

#Puts all uncommited changes in a file changes.log
function changes()
{
git diff > changes.log
}

#Puts each line from every file of your project with tag #TODO into a file todo.log
function todo()
{
grep -r --exclude={todo.log,ProjectAnalyze.sh,changes.log,error.log} "#TODO" * > todo.log
}

#Checks all haskell files for syntax errors and puts the result into error.log
function error()
{
find . -name "*.hs" -exec ghc -fno-code {} \; > error.log
}


#Ask user if they want to view any log file
function displayLogs()
{
echo "Your changes, haskell errors and TODO tags have been logged into log files, do you wish to view them?(Y/N)"
read input
if [ $input == "Y" ]; then
	echo "Type C if you wish to view changes.log, type E if you wish to view errors.log and type T to view TODO.log. Type A if you wish to view your log files at once"
	read type
	if [ $type == "C" ]; then
		echo "*****Processing changes.log*****"
		cat changes.log
	elif [ $type == "E" ]; then
		echo "*****Processing error.log*****"
		cat error.log
	elif [ $type == "T" ]; then
		echo "*****Processing todo.log*****"
		cat todo.log
	elif [ $type == "A" ]; then
		echo "Processing changes.log..."
		cat changes.log
		echo "**********"
		echo "Processing todo.log..."
		cat todo.log
		echo "**********"
		echo "Processing errors.log..."
		cat error.log
		echo "**********"
	fi
elif [ $input == "N" ]; then
echo "Oh alright"
fi
}

#Shows user a three day weather report visual (refer to https://github.com/chubin/wttr.in for more details)
function weather()
{
echo "Hey would you like to check the weather before you step out?(Y/N)"
read weather
if [ $weather == "Y" ] ; then
	curl wttr.in
elif [ $weather == "N" ] ; then
	echo "Oki doki, have a great day"
fi
}

#Calling the functions 
checkStatus
changes
error
todo
displayLogs
weather

