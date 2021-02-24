#! /bin/bash
# This is an unclever play on words with the filename "integrate.sh". This script undoes closes the work done for a function.

feature_branch=$(git branch --show-current)

echo "On branch $feature_branch. Switching to develop"
git checkout develop

echo -e "\nMerging develop with $feature_branch."
echo -en "Enter chapter number: "
read chap_num
git merge $feature_branch -m "Merged $feature_branch with develop (#$chap_num)"
git branch --delete $feature_branch
git log --graph --pretty=format:'%C(yellow)%d%Creset %s %Cgreen(%cr)' -n 10