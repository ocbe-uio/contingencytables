#! /bin/bash
# This script automates the preparation of a script file for integrating the package.

# ======================================================== #
# Moving file to package                                   #
# ======================================================== #

echo 'Moving file from aux/ to R/'
path_filename=$1
mv -v $path_filename R/

filename_ext=$(echo $path_filename | cut -d '/' -f 3)
filename=$(echo $filename_ext | cut -d '.' -f 1)

# ======================================================== #
# Changing branch + initial commit                         #
# ======================================================== #

echo -e "\nCreating $filename branch"
git branch $filename

git checkout $filename

echo -e "\nCommitting original script"
git add -A
git commit -m "Added original script"

# ======================================================== #
# Final conveniences                                       #
# ======================================================== #

echo $filename | xclip -selection clipboard
echo $filename

echo "Run the following in R to reformat the function:"
echo "contingencytables:::reformat('R/$filename_ext')"