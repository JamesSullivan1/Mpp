#!/usr/bin/bash

function usage {
    echo "Usage: $1 [q|l]"
    echo "q - Quiet mode"
    echo "l - Loud mode, emit all ASTs as \$file.out"
}

if [ "$#" -ne 1 ]; then
    usage $0
    exit
fi

rm ./M++tests/*.out 2>/dev/null

if [ "$1" = "q" ]; then
    for f in ./M++tests/*
    do
        if ./mpp $f /dev/null 2>/tmp/err; then
            :
        else
            err=$(</tmp/err)
            echo "Failed on file $f, error : $err"
            exit
        fi
    done
    echo "All tests passed."
elif [ "$1" = "l" ]; then 
    for f in ./M++tests/*
    do 
        if ./mpp $f $f.out; then
            :
        else
            err=$(</tmp/err)
            echo "Failed on file $f, error : $err"
            exit
        fi
    done
    echo "All tests passed."
else
    usage $0
    exit
fi


