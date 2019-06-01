
## Libraries ====
# install.packages("foreach")
# install.packages("expm")
# install.packages("ctmcd")
# install.packages("lubridate")
# install.packages("ggplot2")
# install.packages("doSNOW")
# install.packages("parallel")
require("foreach")
require("expm")
require("ctmcd")
require("lubridate")
require("ggplot2")
require("doSNOW")
require("parallel")

# No of cores for parallel computations
# cl <- makeCluster(2) #change the 2 to your number of CPU cores
# registerDoSNOW(cl)




## Function 1 ====
# Randomly generates new ratings based on the current ratings and 
# transition matrix
# Input: vec - vector of initial ratings, mm - migration matrix
# Output: new.vec - new ratings generated randomly according to
#         the transition matrix
return.migrs <- function(vec, mm) {
  #cum.m <- t(apply(t(mm), 2, cumsum))
  cum.m <- mm
  cum.m[, 2] <- cum.m[, 1] + mm[, 2]
  cum.m[, 3] <- cum.m[, 2] + mm[, 3]
  cum.m[, 4] <- cum.m[, 3] + mm[, 4]
  cum.m[, 5] <- cum.m[, 4] + mm[, 5]
  
  probs <- runif(length(vec))
  new.vec <- rep(NA, length(vec))
  cumm <- cum.m[vec, ]
  if (length(vec) > 1) {
    my.bool <- cumm < probs
    new.vec <- my.bool[,1] + my.bool[,2] + my.bool[,3] + my.bool[,4] + my.bool[,5] + 1
  } else {
    new.vec <- sum(cumm < probs) + 1
  }
  return(as.integer(new.vec))
}




## Function 2 ====
# Transforms migration matrix to transition matrix
# Input: mm - migration matrix
# Output: transition matrix
migr.to.tr <- function(mm) return(mm / apply(mm, 1, sum))




## Function 3.1 ====
# Builds a migration matrix using cohort approach
# Input: v1 - ratings before the migration
#        v2 - ratings after the migration
get.m.coh <- function(v1, v2, x = max(max(v1), max(v2))) {
  mm <- matrix(0, x, x)
  for (i in 1:length(v1)) mm[v1[i], v2[i]] <- mm[v1[i], v2[i]] + 1
  return(migr.to.tr(mm))
}




## Function 3.2 ====
# Builds a migration matrix using cohort approach
# Input: v1 - ratings before the migration
#        v2 - ratings after the migration
get.m <- function(v1, v2, x = max(max(v1), max(v2))) {
  mm <- matrix(0, x, x)
  for (i in 1:length(v1)) mm[v1[i], v2[i]] <- mm[v1[i], v2[i]] + 1
  return(mm)
}




## Function 4 ====
# Finds left or right border of a 2-side Wald confidence interval 
# Input: PD - Expected probability of default
#        N - Number of observations
#        type - left interval border for "l", right otherwise
#        alpha - confidence level
# Output: left or right border of 2-sided confidence interval
CI.W.bord <- function(PD, N, type, alpha = .05) {
  if (type == "l") {kappa <- qnorm(alpha / 2)} else {kappa <- qnorm(1 - alpha / 2)}
  return(min(max(PD + kappa * sqrt(PD * (1-PD) / N), 0), 1))
}




## Function 5 ====
# Finds 2-side Wald confidence interval
# Input: PD - Expected probability of default
#        N - Number of observations
#        alpha - confidence level
# Output: 2-sided confidence interval and expected value
CI.W <- function(PD, N, alpha = .05) {
  return(c(CI.W.bord(PD, N, "l", alpha),   # left border
           PD,
           CI.W.bord(PD, N, "r", alpha)))
}




## Function 6 ====
# Makes a bootstrap interval for the transition matrix
# Input: v1 - ratings before the migration
#        v2 - ratings after the migration
#        alpha - confidence level
#        n - number of bootstrap samples
# Output: 2-sided confidence interval and expected value
CI.Bootstrap <- function(v1, v2, alpha = .05, n = 10000) {
  all.matr <- array(NA, c(5, 5, n))
  for (i in 1:n) {
    new.order <- sample(1:length(v1), replace = T)
    all.matr[,,i] <- get.m.coh(v1[new.order], v2[new.order], 5) 
  }
  res <- array(NA, c(5,5,3))
  res[,,1] <- apply(all.matr, c(1,2), function(x) quantile(x, alpha/2, na.rm = T))
  res[,,2] <- apply(all.matr, c(1,2), mean)
  res[,,3] <- apply(all.matr, c(1,2), function(x) quantile(x, 1 - alpha/2, na.rm = T))
  return(res)
}




## Function 7 ====
# Returns ratings of the clients for "steps" fractions of time
get.J.from.Q <- function(tr.m, v1, v2, steps = 100) {
  Js <- data.frame(matrix(NA, length(v1), steps))
  Js[, 1] <- v1
  mm <- expm::expm(ctmcd::gm(tr.m, te = 1, method="WA")$par / steps)
  lines <- rep(T, length(v1))
  
  # rejection sampling - repeat until everything fits!
  while (sum(lines) > 0) { 
    for (i in 2:steps) Js[lines, i] <- return.migrs(Js[lines, i-1], mm)
    lines <- (Js[, steps] != v2)
  }
  return(Js)
}




## Function 8 ====
# Returns transition matrix using duration method
get.Q.from.J <- function(df, x = max(df), steps = 100) {
  R <- c()
  # Logic is the following: there are 100 steps and 99 periods
  # Hence, the 1st and the last columns have weight of .5, all others - 1
  # We sum them up and devide by # of periods (99), since the time period = 1
  for (j in 1:5) R[j] <- (sum(df[, 2:(steps-1)] == j) + sum(df[, c(1, steps)] == j)/2 )/(steps-1)
  
  my.trans.m <- matrix(0, 5, 5)
  # Now we calculate the number of all transitions
  for(j in 1:(steps-1)) my.trans.m <- my.trans.m + get.m(df[,j], df[,j+1])
  
  # Generator matrix without adjustments (MLE formula)
  tr.m.new <- matrix(rgamma(25, 
                            shape = my.trans.m + 1.1 - 1, 
                            scale = t(matrix(1/(rep(R, each = 5) + 1.1),5,5))),
                     5, 5)
  
  # Adjustments to the generator
  tr.m.new[1, 1] <- -sum(tr.m.new[1, 2:5])
  tr.m.new[2, 2] <- -sum(tr.m.new[2, c(1, 3, 4, 5)])
  tr.m.new[3, 3] <- -sum(tr.m.new[3, c(1, 2, 4, 5)])
  tr.m.new[4, 4] <- -sum(tr.m.new[4, c(1, 2, 3, 5)])
  tr.m.new[5, ] <- c(0,0,0,0,0)
  
  # Transition matrix from generator
  tr.m.new <- expm::expm(tr.m.new)
  for(j in 1:5) tr.m.new[j, ] <- tr.m.new[j, ] / sum(tr.m.new[j, ])
  
  return(tr.m.new)
}




## Function 9 ====
CI.MCMC <- function(v1, v2, alpha = .05, steps = 100, n = 1000) {
  # list of matrices estimated using BMCMC
  gibbs.m <- array(NA, c(5, 5, n))
  
  # the 1st matrix is estimated using the Cohort method
  gibbs.m[,,1] <- get.m.coh(v1, v2)
  
  # all other matrices are obtained using the BMCMC
  for (i in 2:n) {
    ## Find (J | Q)
    temp.df <- get.J.from.Q(gibbs.m[,,i-1], v1, v2, steps = steps)
    
    ## Find (Q | J)
    gibbs.m[,,i] <- get.Q.from.J(temp.df, 5, steps = steps)
  }
  res <- array(NA, c(5,5,3))
  res[,,1] <- apply(gibbs.m, c(1,2), function(x) quantile(x, alpha/2))
  res[,,2] <- apply(gibbs.m, c(1,2), mean)
  res[,,3] <- apply(gibbs.m, c(1,2), function(x) quantile(x, 1 - alpha/2))
  return(res)
}




## Function 9 ====
CI.MCMC.by.path <- function(x, alpha = 0.05) {
  res <- array(NA, c(5, 5, 3))
  
  res[,,1] <- apply(x, c(1,2), function(x) quantile(x, alpha/2))
  res[,,2] <- apply(x, c(1,2), mean)
  res[,,3] <- apply(x, c(1,2), function(x) quantile(x, 1 - alpha/2))
  return(res)
}
