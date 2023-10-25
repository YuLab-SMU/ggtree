#!/bin/bash

pkg=$(basename `pwd`) 
oldver=RELEASE_3_17
newver=RELEASE_3_18

## rm old release branch
make rmrelease

## merge change from bioc
make update

## set current release version
sed -i "s/$oldver/$newver/g" Makefile

## add release in NEWS.md
ver=$(head DESCRIPTION |grep Version  |cut -d' ' -f2)
echo -e "# $pkg $ver\n\n+ Bioconductor $newver ($(date +'%Y-%m-%d, %a'))\n" > tmp.md
cat NEWS.md >> tmp.md 
mv tmp.md NEWS.md


## commit and push to github
git add .
git commit -m 'update bioc version'
git push

## pull current release branch
make release

## go back to devel branch
git checkout devel
