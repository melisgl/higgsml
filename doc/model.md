Winning Model Documentation
---------------------------

Name: Gábor Melis

Location:
    Country: Hungary
    Postal code: 2049
    City: Diósd
    Address: Éva u. 9

Email: mega@retes.hu
Web: http://quotenil.com

Competition: Higgs Boson Machine Learning Challenge


## Summary

The model is a bag of 70 dropout neural networks produced by 2-fold
stratified cross-validations repeated 35 on random shuffles of the
training data.


## Features Selection / Extraction

All features were normalized to have zero mean, stddev 1. The *-phi
features were dropped, because they increased overfitting a lot. A
couple of invariant and transverse mass based features were added (see
ENCODE-EXTRA-MASS in the code) but they didn't bring huge gains. The
same can be said of the extra modulus feature and the features based
on the absolute value of differences of pri_{tau,lep,met}_phi (see
ENCODE-EXTRA-MODULI and ENCODE-EXTRA-RADIAN, respectively). Features
with long tails were log transformed. I tried XGboost and got 3.75+
AMS in local CV with it. With the Cake features, that improved to
3.80+. In the end, I decided not to use the Cake features for fear of
non-reproducability and insufficient testing although even the bag of
neural networks plus bag of xgboost models seemed to benefit about
0.005.


## Modeling Techniques and Training

Every neural network in the bag predicts the probability of an example
being Signal. The probabilities predicted by networks in the bag were
simply averaged and the top 15.2% of the examples were predicted to be
S. This cutoff value was hand selected based on the AMS vs cutoff
curve in cross-validation.

Each neural network had three hidden layers of 600 neurons each and a
2 neuron softmax output layer corresponding to the Signal and
Background classes. The network was trained with cross-entropy loss
and backpropagation.

The hidden layers had max-channel activation functions with a group
size of 3. This means that in each group of 3 neurons, only the one
with max activation fired and the rest were zeroed. See papers

    'Compete to Compute' by Srivastava et al.

    'From Maxout to Channel-Out: Encoding Information on Sparse
    Pathways' by Qi Wang and Joseph Jaja

According to cross-validation results, max-channel worked a bit better
than maxout which was in turn significantly better than rectified
linear units.

In addition to regularization with dropout with the usual rate of 0.5,
an L1 penalty of 0.000005 and an L2 penalty of 0.00005 was applied to
the input-to-first-hidden-layer weights. Each neuron in the first
hidden layer was connected only to 10 inputs (sampled with
replacement before training started).


## Code Description

Everything is in Common Lisp, except for some shell scripts. The most
important files:

src/higgs-boson.lisp   -- general data structures, evaluation
src/csv.lisp           -- reading/writing the csv files
src/bpn.lisp           -- CV bagging of backprop networks
src/encoder.lisp       -- normalization, extra features
src/main.lisp          -- gluing everything together
README.md              -- installation guide
doc/model.md           -- this document
rumcajsz               -- prebuilt binary
configure              -- script to set up build environment and file locations
SETTINGS               -- the file produced by configure

The code is way larger than to be possibly described here in any
detail. I cleaned it up, removed much dead code and added some
comments.

The other files and directories are only relevant to development:

Makefile
build/                 -- build scripts
*.asd                  -- project definition
quicklisp/             -- Common Lisp libraries
submission/            -- default location for generated submissions
test/                  -- test sources
data/                  -- default location for data files
model/                 -- default location for saved models
xgboost-scripts/       -- scripts to train and run xgboost models


## Dependencies

See the separate README file.


## How To Generate the Solution

See the separate README file.


## Additional Comments and Observations

The master plan was to breed features with an evolutionary algorithm
but this didn't work. Ensembling with xgboost seemed a bit better in
cross-validation but at the chosen cutoff scored a bit lower on the
private leaderboard. There were many other things that didn't work,
among them:

- Finer grained ensembling based on various proxy losses.

- Nesterov momentum (increased overfitting).

- Direct optimization of the AMS (overfitting).

- Optimization by AUC (much slower, worse results).

- Pseudo labeling.

The key to this competition has been finding a reliable way to measure
performance and avoiding overfitting. My local CV indicated 3.85ish
AMS so I'm not sure how well that worked. All in all, I feel that the
datasets were way too small for this contest due to the choice of AMS.
The AMS vs cutoff curves of private test data would be great to see,
but even they may indicate lottery taking place.


## Simple Features and Methods

To save time one can train about 8-16 neural networks as opposed to
the 70 that went into the winning submission.


## Figures

See cv-ams-vs-cutoff.png for how the AMS varies against the cutoff
threshold.


## References

Dropout:

    Improving neural networks by preventing co-adaptation of feature detectors
    http://arxiv.org/pdf/1207.0580.pdf

Max-channel:

    'Compete to Compute' by Srivastava et al.

    'From Maxout to Channel-Out: Encoding Information on Sparse
    Pathways' by Qi Wang and Joseph Jaja
