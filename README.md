This is the winning solution to the The Higgs Boson Machine Learning
Challenge on [Kaggle][1] by [Gábor Melis][2].

  [1]: http://www.kaggle.com/c/higgs-boson
  [2]: http://quotenil.com
  [3]: mailto:mega@retes.hu


## Instructions for the batteries included distribution

For the contest one has to submit a zip file with all the libraries
and reproduce the submitted results exactly. The instructions in this
section relate to that.

The winning submission was a bag of 70 dropout neural networks and
took one day to train on a GTX Titan GPU in double precision mode.
Prediction took about an hour.

My tests indicate that 8-16 neural networks are very close in
performance to 70, so this was probably an overkill.


### Prerequisites

Summary of what's needed:

- NVIDIA CUDA Toolkit (tested with 5.5.22, build 331.67)
- a CUDA card, preferably a Titan (in double precision mode) or better
- to rebuild the executable, SBCL (a Common Lisp compiler)
- 24GiB RAM
- about 4GiB of disk space

Let's see why these things are needed. The CUDA card and the NVIDIA
CUDA Toolkit are prerequisites, because training of the dropout
network was performed on a CUDA card relying on the CURANDOM random
number generator, and also because the results of deterministic
operations on the GPU and the CPU can and do differ within the
constraints of the IEEE standard.

A Titan or better is recommended because it is many times faster than
GTX 480 or similar alternatives in double precision mode. I haven't
tested with single precision float, it's likely to work just as well.

SBCL is only necessary to rebuild the executable. SBCL 1.1.14 was used
in the submission, but any recent version should work. 

I'm not sure about the actual minimum memory required, it may work
with 8GiB, but it was tested with 24GiB and that's the heap size with
which the executable was built.


### Reproducing the results

The zip file contains an executable built on a recent Debian Linux
x86-64 Testing installation. This is executable is the file 'rumcajsz'
in the top-level directory.

If this executable works, there is no need to build anything, training
and prediction can be done right away:

        ./higgsml-train <path/to/training.csv> <save-dir-for-trained-model>
        ./higgsml-run <path/to/test.csv> <save-dir-for-trained-model> \
            <path/to/submission.csv>

Note that higgsml-train logs its doings to stdout and also to

         <save-dir-for-trained-model>/rumcajsz.log

If higgsml-train fails with a library or ELF format related error,
then the executable may not be compatible with slc5 and it needs to be
rebuilt with:

        ./higgsml-build

After rebuilding the executable, the above higgsml-{train,run}
commands should work.

On the public and private leaderboards the submission achieved 3.78457
and 3.80581, respectively. Cross-validation on the training set
indicated 3.82-3.85 depending on how much smoothing was applied to the
ams-vs-cutoff curve. For the raw curve see doc/cv-ams-vs-cutoff.png.


## Instructions for the source distribution

1. Install either [SBCL][4] or [AllegroCL][5]. SBCL is free and is
   available on all Linux distributions.

  [4]: http://www.sbcl.org
  [5]: http://franz.com/products/allegro-common-lisp/

2. Set some options:

        $ ./configure --help
        Usage: ./configure
                  [--lisp [ acl | sbcl]]
                  [--sbcl-bin <sbcl-binary>]
                  [--sbcl-options <sbcl-options>]
                  [--acl-bin <acl-binary>]
                  [--acl-options <acl-options>]
                  [--data-dir <directory>]
                  [--model-dir <directory>]
                  [--submission-dir <directory>]

        --lisp
          defaults to 'sbcl'.
        --sbcl-bin
          Path to the sbcl executable. Defaults to 'sbcl'.
        --sbcl-options
          Defaults to '--dynamic-space-size 24000 --noinform
                       --lose-on-corruption --end-runtime-options
                       --non-interactive --no-userinit --no-sysinit
                       --disable-debugger'.
        --acl-bin
          Path to the AllegroCL executable. Defaults to 'alisp'.
        --acl-options
          Defaults to ''.
        --data-dir
          Where the uncompressed csv files reside, defaults to 'data/'.
        --model-dir
          Where the model files are saved, defaults to 'model/'.
        --submission-dir
          Where submission files are saved, defaults to 'submission/'.

The directories can be absolute filenames or relative to the
configuration script.


### Installing dependencies to a project local quicklisp dir

The following command will set up a new quicklisp directory right
below the top-level directory and fetch all dependencies with the exact
versions with which this program was tested:

        make quicklisp


### Installing dependencies to a global quicklisp dir

Well, you are on your own, but you may want to use the following
command to fetch the dependencies not in quicklisp:

        (cd ~/quicklisp/local-projects &&
             <path-to-here>/build/install-local-projects.sh)

If you are having trouble, not that the rest of the libraries were
from the 2014-09-14 quicklisp distribution. See
build/install-dependencies.lisp about how to get historical
distributions.


### Running and Developing

Place training.csv and test.csv into the data directory (by default
./data/).

Train the model and create the leaderboard predictions files by
evaluating the commented out form at the bottom of src/bpn.lisp.

Also see the description of the command line scripts
higgsml-{build,train,run} above.
