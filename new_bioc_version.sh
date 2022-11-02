#!/bin/bash

pkg=$(basename `pwd`) 
oldver=RELEASE_3_15
newver=RELEASE_3_16

## rm old release branch
make rmrelease

## merge change from bioc
make update

## set current release version
sed -i "s/$oldver/$newver/g" Makefile

## add release in NEWS.md
echo "# $pkg\n\n+ Bioconductor $newver ($(date +'%Y-%m-%d, %a'))\n\n" > tmp.md
cat NEWS.md >> tmp.md 
mv tmp.md NEWS.md


## commit and push to github
git add .
git commit -m 'update bioc version'
git push

## pull current release branch
make release

## go back to master branch
git checkout master
