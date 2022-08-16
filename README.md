# EDF: Explicitly Deweighted Features, for Fair ML

## Overview

EDF reduces the impact of each feature in the proxies of sensitive variables in X variebles with a different amount ofdeweighting applied to each such feature,
as described in [our paper](https://t.co/8797n3znGB).

## Installation

The package depends on NM's 
[qeML package ("Quick and Easy Machine Learning"](https://t.co/8797n3znGB), soon to be uploaded to CRAN but for now fully usable at
[NM's GitHub repo site](https://github.com/matloff/qeML).  Both should
be easy to install, but let NM know if any issues.

## Example

``` r

library(EDFfair) 
library(qeML) 
data(pef)  # census data
names(pef)
# [1] "age"     "educ"    "occ"     "sex"     "wageinc" "wkswrkd"

```

Let's see if we can predict income, using sex as the sensitive (and thus
excluded) variable, but treat occupation as the proxy.  (See the paper
for explanation of the gendered nature of occupation.)

Let's try a linear model.  Note that larger values of the deweighting
parameter correspond to the fairness end of the Fairness/Utility
Tradeoff.  Smaller values move toward the utility end (increased
prediction power).  Here is the code (output in commented lines):

``` r

z <- qeFairRidgeLin(data=pef,yName='wageinc',deweightPars=list(occ=0.2),sensNames='sex')
# holdout set has  1000 rows
z$testAcc
# [1] 26653.16

```

Mean Absolute Prediction Error on the holdout set (default: 1000 rows)
was about $27,000.  That's out measure of utility.  What about fairness?

``` r

z$corrs
#       sex
# 0.1979095


```

The predicted Y, given X had a squared correlation with the probability
of female of just under 0.20.  Now let's try, say, a deweighting value
of 1.0:

``` r

z <- qeFairRidgeLin(data=pef,yName='wageinc',deweightPars=list(occ=1.0),sensNames='sex'); c(z$testAcc,z$corrs)
# holdout set has  1000 rows
#                       sex
# 2.555011e+04 1.861465e-01

```

Here both MAPE and fairness improved a little.  One should note, though,
that one should try many holdout sets, say 100 of them:

``` r

replicMeans(100,"{z <- qeFairRidgeLin(data=pef,yName='wageinc',deweightPars=list(occ=1.0),sensNames='sex'); c(z$testAcc,z$corrs)}")
#                       sex 
# 2.553578e+04 2.243124e-01 
# attr(,"stderr")
#                       sex 
# 1.092683e+02 2.749224e-03 


```

The **replicMeans()** function is from the **regtools** package on CRAN,
included by **qeML**.  Here we tried 100 random holdout sets.  The
average MAPE was $25535.78, close to our value above for a single
holdout set, with a standard error of about $109.  (To get an
approximate 95% confidence interval, we add and subtract 1.96 times this
value to $25535.78.)

However, the single-holdout set run we had below seems to have been
overly optimistic; the true fairness valuee is about 0.22.

We may then try to add another proxy varialble to occupation, say the
number of weeks worked:

``` r

z <- qeFairRidgeLin(data=pef,yName='wageinc',deweightPars=list(occ=1.0,wkswrkd=0.4),sensNames='sex'); c(z$testAcc,z$corrs)


```

Or, we could try random forests instead of a linear model.  Note that
here (and with the k-Nearest Neighbors function), smaller values of the
deweighting parameters mean more fairness.

