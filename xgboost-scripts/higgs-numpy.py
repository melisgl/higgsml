#!/usr/bin/python
# this is the example script to use xgboost to train 
import inspect
import os
import sys
import numpy as np
import scipy
import random
# add path of xgboost python module
code_path = os.path.join(
    os.path.split(inspect.getfile(inspect.currentframe()))[0], "../xgboost/python")
sys.path.append(code_path)
import xgboost as xgb

test_size = 550000

# load in training data, directly use numpy
seed = int(sys.argv[1])
labelcolumn = int(sys.argv[3])
dtrain = np.loadtxt( sys.argv[2], delimiter=',', skiprows=1, converters={labelcolumn: lambda x:int(x=='s'.encode('utf-8')) } )
print ('finish loading from csv ')

ncols = dtrain.shape[1]
label  = dtrain[:,ncols-1]
data   = dtrain[:,1:ncols-2]
# rescale weight to make it same as test set
weight = dtrain[:,ncols-2] * float(test_size) / len(label)

sum_wpos = sum( weight[i] for i in range(len(label)) if label[i] == 1.0  )
sum_wneg = sum( weight[i] for i in range(len(label)) if label[i] == 0.0  )

# print weight statistics 
print ('weight statistics: wpos=%g, wneg=%g, ratio=%g' % ( sum_wpos, sum_wneg, sum_wneg/sum_wpos ))

# construct xgboost.DMatrix from numpy array, treat -999.0 as missing value
xgmat = xgb.DMatrix( data, label=label, missing = -999.0, weight=weight )

# setup parameters for xgboost
param = {}
param['objective'] = 'binary:logistic'
# scale weight of positive examples
param['scale_pos_weight'] = sum_wneg/sum_wpos
param['bst:eta'] = 0.15
# param['bst:subsample'] = 0.5
param['bst:max_depth'] = 6
param['eval_metric'] = 'auc'
param['silent'] = 1
param['nthread'] = 1
param['seed'] = seed
random.seed(seed)

# you can directly throw param in, though we want to watch multiple metrics here 
plst = list(param.items())+[('eval_metric', 'ams@0.15')]

watchlist = [ (xgmat,'train') ]
num_round = 360
print ('loading data end, start to boost trees')
print ('seed %s\n' % seed)
bst = xgb.train( plst, xgmat, num_round, watchlist );
# save out model
bst.save_model('higgs.model')

print ('finish training')
