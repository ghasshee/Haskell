#!/bin/sh 

DIR=~/Programs/hs/Haskell
ls | while read line ; do 
    if [ -d $line ]     
    then
        cd $DIR/$line && [ -e Makefile ] && {
            echo "===================================================================" 
            echo "compling $line ... " 
            echo "-------------" 
        } && make
        cd $DIR
    fi 
done 
