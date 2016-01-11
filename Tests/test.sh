#!/bin/bash
regexNewTest="### TEST ((.)*)###"
regexExpectedResult="^@SHOULD_(NOT_PASS|PASS)$"
regexEnd="^### END ###"
regexEmptyLine="^[\s\S]*$"

currentTime=$(date "+%Y.%m.%d-%H.%M.%S")
file="tempTestFile".$currentTime."java"

text=""
actualDirectory=$(pwd)
expectedResult=""

syntaxChecker=0
lineNumber=1
totalTestNumber=0
totalFailNumber=0

#Checking arguments
if [ "$#" -ne 2 ]; then
    printf "Illegal number of parameters \n"
    printf "usage command pathToTestFile pathToProjectDirectory \n"
    exit 1
fi

#Reading test file line by line.
while IFS='' read -r line || [[ -n "$line" ]]; do
	#If the line match a test end, executes the test.
	if [[ $line =~ $regexEnd ]]; then

		#Checking test file syntax.
		if [ "$syntaxChecker" -eq "3" ]; then
			syntaxChecker=0
		else
			printf "ERROR unexpected end test line $lineNumber \n"
			exit 1
		fi

		#Writing test content into temp file.
		echo $text > $file
		text=""
		#Switching to project directory.
		cd $2
		#Executing test.
		ocamlbuild Main.byte -- $actualDirectory"/"$file &>/dev/null
		#Retrieving result.
		parsingResult=$?
		#Do things according to test result and expected result.
		if  [ "$expectedResult" = "PASS" ]; then
			if [ "$parsingResult" -eq "0" ]; then
				printf "SUCCESS!\n"
			else
				printf "FAIL!\n"
				#Increments the number of fails.
    			((totalFailNumber=totalFailNumber+1))
			fi
		else
			if [ "$parsingResult" -eq "0" ]; then
				printf "FAIL!\n"
				#Increments the number of fails.
    			((totalFailNumber=totalFailNumber+1))
			else
				printf "SUCCESS!\n"
			fi
		fi
		printf "\n"
		cd $actualDirectory

	elif [[ $line =~ $regexNewTest ]]; then

		#Checking test file syntax.
		if [ "$syntaxChecker" -eq "0" ]; then
			syntaxChecker=1
		else
			printf "ERROR unexpected new test line $lineNumber \n"
			exit 1
		fi

		#Increments the number of tests.
    	((totalTestNumber=totalTestNumber+1))

		#Gets the description of the test.
		description=${BASH_REMATCH[1]}
		printf "New Test : $description\n"

	elif [[ $line =~ $regexExpectedResult ]]; then

		#Checking test file syntax.
		if [ "$syntaxChecker" -eq "1" ]; then
			syntaxChecker=2
		else
			printf "ERROR unexpected definition of expected result line $lineNumber \n"
			exit 1
		fi

		#Sets the value of the expected result.
		expectedResult=${BASH_REMATCH[1]}
		printf "Next test should $expectedResult : "

    else
    	#Checking test file syntax.
    	#Taking care of empty lines between tests.
    	if [ "$syntaxChecker" -eq "2" ]; then
			syntaxChecker=3
		elif [ "$syntaxChecker" -eq "3" ]; then
			syntaxChecker=3
		elif [ "$syntaxChecker" -eq "0" ]; then
			if ! [[ $line =~ $regexEmptyLine ]]; then
				printf "WARNING unexpected data line $lineNumber \n"
				continue
			fi
		else
			printf "ERROR unexpected test content line $lineNumber \n"
			exit 1
		fi
    	text=$text$line
    fi
    #Increments the number of lines.
    ((lineNumber=lineNumber+1))
done < "$1"
#Clears temp file.
rm -f $file
printf "$totalTestNumber tests : $totalFailNumber failures. Have a good day Sir!\n"