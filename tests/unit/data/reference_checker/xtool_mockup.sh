#!/bin/sh

command="$*"
# we parse the command to extract the tolerance value after the -to flag
number=$(echo "$command" | grep -oP '\-to\s+\K\d+')
echo tolerance:$number

f1=$(echo "$command" | grep -oP '\-f1\s+\K\S+')
echo f1:$f1

f2=$(echo "$command" | grep -oP '\-f2\s+\K\S+')
echo f2:$f2

if [ -n "$number" ]; then
    if [ "$number" -le 10 ]; then
        echo "xtool : OK - Files differ but tolerance is acceptable"
        exit 0
    else
        echo "xtool : FAILURE - Files differences greater than tolerance"
        exit 1
    fi
else
    echo "xtool : ERROR - Invalid command format"
    exit 2
fi
