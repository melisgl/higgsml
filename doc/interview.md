## What was your background prior to entering this challenge?

Although I did specialize in AI at the university, [I
am](http://quotenil.com) still very much an autodidact when it comes
to machine learning. Spent a good chunk of my life writing
five-in-a-row players, Hex AI ([Six](http://quotenil.com/hex/six/)),
and entered several contests (Netflix, Google AI Challenges) including
ones on Kaggle.

Also, I worked professionally for a couple of years on the boundary of
natural language processing and automated trading, doing sentiment
analysis, named entity recognition, etc.


## What made you decide to enter?

It was the irresistible pull of the Higgs Boson. I also had a low
labor in mind. What almost made me decide to withdraw was the
realization of noisy the evaluation function is, but it was too late,
I was hooked.


## What preprocessing and supervised learning methods did you use?

I only added a few handcrafted features based on the transverse and
invariant mass formulas given by the organizers, plus a few that had
to do with the minimum transverse angle between tau, lep, met.
Features with long tails were log-transformed.


## What was your most important insight into the data?

I wish I had had one.


## Were you surprised by any of your insights?

Mostly by the almost continuous lack of insights.


## Which tools did you use?

Common Lisp (developed on
[AllegroCL](http://www.franz.com/products/allegrocl/), a commercial
implementation; submitted with [SBCL](http://sbcl.org), a free one)
that allows for rapid exploration and is fast.

I used [MGL](http://cliki.net/MGL), my machine learning library, for
the backprop stuff which is based on Masayuki Takagi's excellent
CL-CUDA library.

I also used MGL-GPR, a library of evolutionary algorithms, for all the
failed experiments the outsmart the physicists by breeding better
features and thus went the low-labor dreams.


## What have you taken away from this competition?

That feature engineering is not always the key. Reliably evaluating
solutions locally is. Determining the relative importance of local
cross-validation results and the public leaderboard scores is the next
step.

I failed to construct a good fitness function for the evolutionary
algorithms and saw training and test scores diverge all the time. This
taught me not to trust the peak AMS, the peak of the smoothed AMS, the
area under the AMS curve, etc. In the end, I only accepted changes
that improved the AMS if they dominated the old version in almost all
regions of the AMS curve at very levels of smoothing and I could
explain why it would be better.

Another take-away is that it pays off to improve the infrastructure. I
couldn't have won this competition if I hadn't spent the time earlier
on the CUDAization of MGL.
