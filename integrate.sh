#! /bin/bash
# This script automates the preparation of a script file for integrating the package.

echo 'Moving file from aux/ to R/'
path_filename=$1
mv -v $path_filename R/

filename_ext=$(echo $path_filename | cut -d '/' -f 3)
filename=$(echo $filename_ext | cut -d '.' -f 1)

echo "Creating $filename branch"
git branch $filename

git checkout $filename

echo "Committing original script"
git add -A
git commit -m "Added original script"

echo -e "\nFunction integrated:\n"
echo $filename | xclip -selection clipboard
echo $filename

# Finally, open file in editor
xdg-open R/$filename_ext