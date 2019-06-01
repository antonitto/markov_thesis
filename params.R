## Block 1. Parameters ====
source("functions.R")

# True Generator matrix - example
trans.m <- t(matrix(c(-.050,  .049,  .001,  .000,  .000,
                      .025, -.075,  .049,  .001,  .000,
                      .001,  .024, -.100,  .074,  .001,
                      .000,  .001,  .024, -.100,  .075,
                      .000,  .000,  .000,  .000,  .000), 5, 5))

# Estimate transition matrix from the generator
trans.m <- expm::expm(trans.m)