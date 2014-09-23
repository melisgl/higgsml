#!/bin/sh
#
# Wrapper for allegro cl. Edit this file to change the name or path to
# the ACL binary.

BASE=`dirname "$0"`

. "$BASE/../SETTINGS"

"$ACLBIN" $ACLOPTIONS "$@"
