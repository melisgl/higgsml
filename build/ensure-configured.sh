#!/bin/sh
BASE=$(dirname "$0")
cd "${BASE}/../"
if [ -f "SETTINGS" ]; then
    echo "Reusing existing configuration"
else
    ./configure "$@"
fi
