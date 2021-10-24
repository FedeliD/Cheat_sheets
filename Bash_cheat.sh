#!/bin/bash

##########################
#For loopining
##########################


#for loop
for (( i = 0; i < 10; i++ )); do
  #statements
done

#for number in {1..13}; do
#  for letter in a b c d
#  do
#    echo "number in $number, letter is $letter"
#  done
#done

while [[ condition ]]; do
  #statements
done



##########################
#FUNCTION
##########################

#function name(parameter) {
  #statements
#}

function addseq {
  sum=0
  for element in $@
  do
    let sum=sum+$element
  done

  echo $sum
}

##########################
#IF statements
##########################


if [[ condition ]]; then
  #statements
fi


if [[ $1 -gt 3 ]] && [[ $1 -lt 7 ]]; then
  echo "$1 is between 3 and 7"
elif [[ $1 =~ "Jeff" ]] || [[ $1 =~ "Roger" ]] || [[ $1 =~ "Brian" ]]; then
  echo "$1 works in the Data Science Lab"
else
  echo "You entered $1, not what I was looking for"
fi
