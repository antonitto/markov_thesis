## Data preparation ====

load("experiment_3/experiment_3_1/exp_3_1.RData")
source("functions.R")
require("dplyr")
require("reshape2")

# df for bootstrap
df <- migr.m <- input.data %>%
  select(date, client_id, dpd_grp, dpd_grp_end) %>% 
  unique %>% 
  select(dpd_grp, dpd_grp_end)

# migration matrix for BMCMC
migr.m <- input.data %>%
  select(date, client_id, dpd_grp, dpd_grp_end) %>% 
  unique %>%
  group_by(dpd_grp, dpd_grp_end) %>%
  count %>% 
  dcast(dpd_grp ~ dpd_grp_end)
migr.m <- migr.m[, 2:6]
migr.m[is.na(migr.m)] <- 0
migr.m


## Bootstrap ====
set.seed(12345)
my.Bootstrap <- function(v1, v2, n = 10000) {
  pb <- winProgressBar(title = "progress bar", min = 0,
                       max = n, width = 300)
  all.matr <- array(NA, c(5, 5, n))
  for (i in 1:n) {
    new.order <- sample(1:length(v1), replace = T)
    all.matr[,,i] <- get.m.coh(v1[new.order], v2[new.order], 5) 
    setWinProgressBar(pb, i)
  }
  close(pb)
  return(all.matr)
}
btstrp.smpl <- my.Bootstrap(df$dpd_grp, df$dpd_grp_end, n = 5000)
saveRDS(btstrp.smpl, "experiment_3/experiment_3_1/btstrp_smpl.RDS")


ECL.calc <- function(PDs, LGD, EAD, D, ratings, outstandings) {
  my.PDs <- PDs[ratings]
  ECL <- sum(my.PDs * LGD * D * outstandings)
  return(ECL)
}

ECLs.btstrp <- c()
for (i in 1:length(btstrp.smpl[1,1,])) {
  ECLs.btstrp <- c(ECLs.btstrp, 
                   ECL.calc(PDs = btstrp.smpl[, 5, i],
                            LGD = .65,
                            EAD = .85,
                            D = 1/1.1,
                            ratings = input.data$dpd_grp[input.data$date == "2019-01-01" &
                                                           input.data$dpd_grp != 5],
                            outstandings = input.data$outstanding[input.data$date == "2019-01-01" &
                                                                    input.data$dpd_grp != 5])) 
  print(i)
}
ECLs.btstrp <- ECLs.btstrp / sum(input.data$outstanding[input.data$date == "2019-01-01" &
                                                          input.data$dpd_grp != 5])
# hist(ECLs.btstrp)
  


## BMCMC ====
set.seed(12345)
# prior for simulation
pr = list()
pr[[1]] = matrix(1, 5, 5)
pr[[1]][5, ] = 0
pr[[2]] = c(rep(5, 4), Inf)

# BMCMC Simulation
BMCMC.smpl <- ctmcd::gmGS(
  tmabs = as.matrix(migr.m),
  sampl_method="Unif",
  te = 1,
  prior = pr,
  burnin = 500,
  niter = 5000,
  verbose = TRUE
)
BMCMC.smpl <- lapply(BMCMC.smpl$draws, expm::expm)
BMCMC.smpl <- array(unlist(BMCMC.smpl), dim = c(5, 5, 5000))
saveRDS(BMCMC.smpl, "experiment_3/experiment_3_1/BMCMC_smpl.RDS")


ECLs.BMCMC <- c()
for (i in 1:length(BMCMC.smpl[1,1,])) {
  ECLs.BMCMC <- c(ECLs.BMCMC, 
                   ECL.calc(PDs = BMCMC.smpl[, 5, i],
                            LGD = .65,
                            EAD = .85,
                            D = 1/1.1,
                            ratings = input.data$dpd_grp[input.data$date == "2019-01-01" &
                                                           input.data$dpd_grp != 5],
                            outstandings = input.data$outstanding[input.data$date == "2019-01-01" &
                                                                    input.data$dpd_grp != 5])) 
  print(i)
}
ECLs.BMCMC <- ECLs.BMCMC / sum(input.data$outstanding[input.data$date == "2019-01-01" &
                                                        input.data$dpd_grp != 5])
# hist(ECLs.btstrp)

ggplot() +
  geom_density(aes(ECLs.btstrp), fill = beau_palette[1], alpha = .9) +
  geom_density(aes(ECLs.BMCMC), fill = beau_palette[2], alpha = .95) +
  beau_theme
