## 1. Initial parameters & packages ####
library(coda)
library(ctmcd)
source("functions.R")
trans.m <- t(matrix(c(-.050,  .049,  .001,  .000,  .000,
                      .025, -.075,  .049,  .001,  .000,
                      .001,  .024, -.100,  .074,  .001,
                      .000,  .001,  .024, -.100,  .075,
                      .000,  .000,  .000,  .000,  .000), 5, 5))
trans.m <- expm::expm(trans.m)


## sample generation
my.Ns <- 5000
rs <- matrix(NA, my.Ns, 2)
rs[, 1] <- as.integer(rep(1:5, each = my.Ns / 5))
rs[, 2] <- return.migrs(rs[, 1], trans.m)


## Gibbs sample calculation
# prior distribution
pr = list()
pr[[1]] = matrix(1, 5, 5)
pr[[1]][5, ] = 0
pr[[2]] = c(rep(5, 4), Inf)




## 2. Визуальный анализ ####
## 2.1. BMCMC - классическая схема (10000 + 1000 'burn-in')
gspar <- ctmcd::gmGS(
  tmabs = get.m(rs[, 1], rs[, 2]),
  te = 1,
  prior = pr,
  burnin = 1000,
  niter = 10000,
  verbose = TRUE
)

# list to array
MCMC.res <- array(NA,c(5,5,10000))
for (i in 1:10000) MCMC.res[,,i] <- expm(gspar$draws[[i]])

# Коэффициент на главной диагонали
acf(MCMC.res[1,1,])
pacf(MCMC.res[1,1,])

# Коэффициент вне главной диагонали
acf(MCMC.res[2,3,])
pacf(MCMC.res[2,3,])




## 2.2. BMCMC - укороченная схема без burn-in (5000)
gspar <- ctmcd::gmGS(
  tmabs = get.m(rs[, 1], rs[, 2]),
  te = 1,
  prior = pr,
  burnin = 0,
  niter = 5000,
  verbose = TRUE
)

# list to array
MCMC.res <- array(NA,c(5,5,5000))
for (i in 1:5000) MCMC.res[,,i] <- expm(gspar$draws[[i]])

# Коэффициент на главной диагонали
acf(MCMC.res[1,1,])
pacf(MCMC.res[1,1,])

# Коэффициент вне главной диагонали
acf(MCMC.res[1,1,])
pacf(MCMC.res[1,1,])





## 3. Convergence check - Gelman statistic ####
### Derive Estimate
library(foreach)
library(doParallel)
N <- 1e5
nco <- detectCores()
cl <- makeCluster(nco)
registerDoParallel(cl)
gspar = foreach(i = 1:nco,
                .packages = c("ctmcd", "expm")) %dopar% gm(
                  tm = get.m(rs[, 1], rs[, 2]),
                  te = 1,
                  method = "GS",
                  burnin = 1000,
                  prior = pr,
                  conv_pvalue = 1,
                  niter = N / nco
                )
stopCluster(cl)

parlist <- lapply(gspar, function(x) x$par)
parest <- Reduce('+', parlist) / nco

chainlist <- as.mcmc.list(lapply(gspar, function(x) {
  as.mcmc(do.call(rbind, lapply(x$draws, as.vector)))
}))

parchainlist <- lapply(chainlist,
                       function(x) x[,as.vector(parest) > 0])
gelman.diag(parchainlist)
