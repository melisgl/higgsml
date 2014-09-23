#!/usr/bin/python
# make prediction 
import inspect
import os
import sys
import numpy as np
import scipy
# add path of xgboost python module
code_path = os.path.join(
    os.path.split(inspect.getfile(inspect.currentframe()))[0], "../xgboost/python")
sys.path.append(code_path)
import xgboost as xgb

modelfile = 'higgs.model'
outfile = sys.argv[2]

# load in training data, directly use numpy
dtest = np.loadtxt( sys.argv[1], delimiter=',', skiprows=1 )
ncols = dtest.shape[1]
data   = dtest[:,1:ncols]
idx = dtest[:,0]

print ('finish loading from csv ')
xgmat = xgb.DMatrix( data, missing = -999.0 )
bst = xgb.Booster({'nthread':8})
bst.load_model( modelfile )
ypred = bst.predict( xgmat )

res  = [ ( int(idx[i]), ypred[i] ) for i in range(len(ypred)) ] 

rorder = {}
for k, v in sorted( res, key = lambda x:-x[1] ):
    rorder[ k ] = len(rorder) + 1

# write out predictions
fo = open(outfile, 'w')
for k, v in res:        
    fo.write('%s %s\n' % ( k,  v) )
fo.close()

print ('finished writing into prediction file')
