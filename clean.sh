#!/bin/sh 

DIR=~/Programs/hs/Haskell
ls | while read line ; do 
    if [ -d $line ]     
    then
        cd $DIR/$line && [ -e Makefile ] && {
            echo "===================================================================" 
            echo "cleaning dir:  $line ... " 
            echo "-------------" 
        } && make clean 
        cd $DIR
    fi 
done 
