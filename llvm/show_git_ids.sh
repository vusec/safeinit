#!/bin/sh
git show | grep git-svn-id
cd tools/clang
git show | grep git-svn-id
cd ../../tools/polly
git show | grep git-svn-id
cd ../../projects/compiler-rt
git show | grep git-svn-id
cd ../..
