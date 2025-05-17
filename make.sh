#!/bin/sh 

DIR=~/Programs/hs/Haskell
ls | while read line ; do 
    pwd
    cd $DIR/$line && make
    cd $DIR
done 
