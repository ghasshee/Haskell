#!/bin/sh 

DIR=~/Programs/hs/Haskell
ls | while read line ; do 
    echo "hoge$line"
    if [ -d $line ]     
    then
        cd $DIR/$line && pwd && make
        cd $DIR
    fi 
done 
