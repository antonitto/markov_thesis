# Experiment 1

Experiment 1 checks the adequacy of 3 methods proposed to build confidence (credibility) intervals for probabilities of default:

* Wald confidence intervals;
* bootstrap confidence intervals;
* BMCMC credibility intervals.

## Experiment 1.1

Under this experiment we assume a true transition matrix and build 10,000 95% confidence intervals for the matrix.
The true matrix should be covered in appr. 95% of cases.

We find that bootstrap and BMCMC provide valid results while Wald method returns convidence intervals that appear to be somewhat narrow on small samples.

## Experiment 1.2

For bootstrap and BMCMC method we test the influence of an imbalanced data sample on the results. It turns out that its effect is insignificant.

## Experiment 1.3

We test the convergence of BMCMC to stationary distribution using visual analysis and Gelman-Rubin statistics. The results show that convergence criterium is met.