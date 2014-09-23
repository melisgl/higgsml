#!/bin/sh
#
# Wrapper for sbcl. Set common command line arguments to sbcl.

BASE=`dirname "$0"`

. "$BASE/../SETTINGS"

"$SBCLBIN" $SBCLOPTIONS "$@"
