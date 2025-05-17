#!/bin/sh 

DIR=~/Programs/hs/Haskell
ls | while read line ; do 
    if [ -d $line ]     
    then
        cd $DIR/$line && pwd && make clean 
        cd $DIR
    fi 
done 
