#!/bin/bash

BASE=$(dirname $0)

python -u ${BASE}/higgs-numpy.py $1 $2 $5
ret=$?
if [[ $ret != 0 ]]; then
    echo "ERROR in higgs-numpy.py"
    exit $ret
fi
python -u ${BASE}/higgs-pred.py $3 $4
ret=$?
if [[ $ret != 0 ]]; then
    echo "ERROR in higgs-pred.py"
    exit $ret
fi
