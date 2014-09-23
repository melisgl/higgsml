#!/bin/sh

BASE=`dirname "$0"`

. "$BASE/../SETTINGS"

if [ "$LISP" = "sbcl" ]; then
    "$BASE/run-sbcl.sh" "$@"
else
    while [ "$1" != "" ]; do
        case $1 in
            --load)
                shift
                load=$1
                ;;
            --eval)
                shift
                eval=$1
                ;;
        esac
        shift
    done
    if [ "$load" != "" -a "$eval" != "" ]; then
        "$BASE/run-acl.sh" -L "$load" -e "$eval"
    elif [ "$load" != "" ]; then
        "$BASE/run-acl.sh" -L "$load"
    elif [ "$eval" != "" ]; then
        "$BASE/run-acl.sh" -e "$eval"
    else
        "$BASE/run-acl.sh"
    fi
fi
