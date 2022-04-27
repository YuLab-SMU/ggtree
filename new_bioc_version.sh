#!/bin/bash

oldver=RELEASE_3_14
newver=RELEASE_3_15

## rm old release branch
make rmrelease

## merge change from bioc
make update

## set current release version
sed -i "s/$oldver/$newver/g" Makefile

## commit and push to github
git add .
git commit -m 'update bioc version'
git push

## pull current release branch
make release

## go back to master branch
git checkout master
