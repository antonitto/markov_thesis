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
my.Ns <- rep(n, m)

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
MCMC.m <- array(NA, c(5, 5, 3, m)) # MCMC results
timer = now()

# Migration matrices
m.m <- array(NA, c(5, 5, length(my.Ns)))
for (j in 1:length(my.Ns)) m.m[,,j] <- get.m(rs[[j]][,1], rs[[j]][,2], 5)

# prior for simulation
pr = list()
pr[[1]] = matrix(1, 5, 5)
pr[[1]][5, ] = 0
pr[[2]] = c(rep(5, 4), Inf)

## Block 2. BMCMC confidence intervals ####
timer <- now()
for (i in 1:m) { 
  print(paste("Iteration:", i))
  temp <- ctmcd::gmGS(
    tmabs = m.m[, , i],
    te = 1,
    prior = pr,
    burnin = 1000,
    niter = 10000,
    verbose = TRUE
  )
  
  # Переводим генераторы в матрицы переходных вероятностей
  temp <- lapply(temp$draws, expm::expm)
  temp <- array(unlist(temp), dim = c(5, 5, 10000))
  MCMC.m[, , , i] <- CI.MCMC.by.path(temp)
  print("")
  print(now() - timer)
}

# Table print
res <- (MCMC.m[,,1,] < array(rep(trans.m), c(5, 5, m))) & (array(rep(trans.m), c(5, 5, m)) < MCMC.m[,,3,])
apply(res, c(1,2), mean)
