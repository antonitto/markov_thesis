## Block 1. Setup ====
source("functions.R")
source("params.R")


# to generate tables 7-9 from the thesis please choose n
# n = 100  -> Table 7
# n = 500  -> Table 8
# n = 1000 -> Table 9
n = 1000

# number of iterations (= length of data by default)
m = 10000 # Consider lowering to spend less time




## Block 2. Data Generation ====

# Sample lengths
my.Ns <- (10:2500) * 20

# Rating migrations
set.seed(12345) # seed set for reproducibility
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





## Block 3. Calculation ====

# Bootstrap results array
bootstrap.m <- array(NA, c(5, 5, 3, m)) # MCMC results
my.list <- list()
timer = now()

# Calculation
for (zet in 1:m) {
  cat(paste("Iteration:", zet, "- "))
  bootstrap.m[,,,zet] <- CI.Bootstrap(rs[[zet]][1:n, 1], 
                                      rs[[zet]][1:n, 2], 
                                      n = 10000)
  print(now() - timer)
}

# Table print
res <- (bootstrap.m[,,1,] < array(rep(trans.m), c(5, 5, m))) & (array(rep(trans.m), c(5, 5, m)) < bootstrap.m[,,3,])
apply(res, c(1,2), mean)
