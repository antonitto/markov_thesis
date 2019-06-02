# Experiment 3

## Experiment 3.1

[Experiment 3.1](experiment_3_1) proposes methodology to conduct confidence estimation of Expected credit loss (ECL) using multivariate distribution of probabilities of default. According to Basel standards and IFRS 9, the general formula for ECL is the following:

$$ECL = PD \cdot LGD \cdot EAD \cdot D,$$

where 

$ECL$ - Expected Credit Loss; 

$PD$ - Probability of Default; 

$LGD$ - Loss Given Default; 

$EAD$ - Exposure at Default; 

$D$ - Discount; 

In a general case we should use a multivariate distribution of all ECL components.
Since we seek to demonstrate how PD distributions could be used in confidence estimation of ECL, in this experiment we assume LGD, EAD and D as constants (65%, 85% and 1/(1 + 10%) respectively).

## Experiment 3.2

Imagine that we have a significant number of states within a Markov chain (26 rating notches or 92 days past due states).
In the most likely case we will not have enough data to estimate all $26 \times 26$ or $92 \times 92$ transition probabilities. In this case we usually group our data in segments (e.g. group "AAA+", "AAA" and "AAA-" rating together in "AAA" group).

In [Experiment 3.2](experiment_3_2) we propose a methodology to tackle the problem of optimal data segmentation. We use the approach proposed in Experiment 3.1 to build ECL confidence interval for each data segmentation and eventually choose the segmentation that minimizes the width of 95% interval for ECL.
