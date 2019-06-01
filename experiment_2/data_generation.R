## Block 0. Parameters ####
# True Generator matrix - example
trans.m <- t(matrix(c(-.050,  .049,  .001,  .000,  .000,
                      .025, -.075,  .049,  .001,  .000,
                      .001,  .024, -.100,  .074,  .001,
                      .000,  .001,  .024, -.100,  .075,
                      .000,  .000,  .000,  .000,  .000), 5, 5))

# Estimate transition matrix from the generator
trans.m <- expm::expm(trans.m)
source("functions.R")

# Sample lengths
# my.Ns <- rep(500, 500)
my.Ns <- 10:2500 * 20
# my.Ns <- rep(5000, 10000)

# Rating migrations
#set.seed(12345) # seed set for reproducibility
rs <- list() # a list of "observed" ratings migrations
for (zet in 1:length(my.Ns)) {
  # for each sample length zet we create a matrix of obs-s before and after the transition
  rs[[zet]] <- matrix(NA, my.Ns[zet], 2)
  
  # The first column is ratings before the transition
  # each rating has an equal (Sum / 5) number of initial observations
  rs[[zet]][, 1] <- as.integer(rep(1:5, each = my.Ns[zet] / 5))
  
  # The second column is randomly generated using the trans. matrix
  rs[[zet]][, 2] <- return.migrs(rs[[zet]][, 1], trans.m)
}

# Экспорт данных
save.image("~/MCplus/200_210_xxx_50000.RData")
