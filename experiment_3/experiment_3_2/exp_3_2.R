require("dplyr")
require("expm")

return.migrs <- function(vec, mm) {
  cum.m <- t(apply(t(mm), 2, cumsum))
  
  probs <- runif(length(vec))
  new.vec <- rep(NA, length(vec))
  cumm <- cum.m[vec, ]
  
  my.bool <- cumm < probs
  new.vec <- rowSums(my.bool) + 1
  
  return(as.integer(new.vec))
}

merge.migrs <- function(vec, dict) {
  return(as.integer(dict[vec]))
}

## Function 2 ####
# Transforms migration matrix to transition matrix
# Input: mm - migration matrix
# Output: transition matrix
migr.to.tr <- function(mm) return(mm / apply(mm, 1, sum))

## Function 3.1 ####
# Builds a migration matrix using cohort approach
# Input: v1 - ratings before the migration
#        v2 - ratings after the migration
get.m.coh <- function(v1, v2, x = max(max(v1), max(v2))) {
  mm <- matrix(0, x, x)
  for (i in 1:length(v1)) mm[v1[i], v2[i]] <- mm[v1[i], v2[i]] + 1
  return(migr.to.tr(mm))
}

get.m <- function(v1, v2, x = max(max(v1), max(v2))) {
  mm <- matrix(0, x, x)
  for (i in 1:length(v1)) mm[v1[i], v2[i]] <- mm[v1[i], v2[i]] + 1
  return(mm)
}




mm <- openxlsx::read.xlsx("matrix.xlsx")
mm <- matrix(unlist(mm[1:92, 2:93]), 92, 92)
mm <- mm %^% 365
mm[,92]

results <- list()

# Первичные данные по DPD
vec.1 <- rep(1L, 1e4)
vec.2 <- return.migrs(vec.1, mm = mm)
vec.3 <- return.migrs(vec.2, mm = mm)

n.iter = 2500



# Сам эксперимент
dicts <- list() 
for (i in c(3, 4, 5, 7, 8, 11, 12, 17, 20, 32)) dicts[[i]] <- c(1, rep(2:(i-1), each = 90/(i-2)), i) 

for (len in c(3, 4, 5, 7, 8, 11, 12, 17, 20, 32)) { 
  vec.2.m <- merge.migrs(vec.2, dicts[[len]])
  vec.3.m <- merge.migrs(vec.3, dicts[[len]])
  est.mm <- get.m(vec.2.m, vec.3.m, len)
  
  ## Example prior parametrization (absorbing default state)
  pr = list()
  pr[[1]] = matrix(1, len, len) # ПОМЕНЯТЬ 5 НА ЧИСЛО
  pr[[1]][len, ] = 0 # ПОМЕНЯТЬ 5 НА ЧИСЛО
  pr[[2]] = c(rep(5, len-1), Inf)  # ПОМЕНЯТЬ 4 НА ЧИСЛО
  
  est.tm <- ctmcd::gmGS(tmabs = est.mm,
                        te = 1,
                        sampl_method = "Unif",
                        prior = pr,
                        burnin = 1000,
                        niter = n.iter,
                        verbose = TRUE)
  results[[len]] <- est.tm
}


# ECL confidence estimation ===============================
ECL.results <- list()
for (i in c(3, 4, 5, 7, 8, 11, 12, 17, 20, 32)) {
  print(paste("Starting:", i))
  
  ECL.results[[i]] <- rep(NA, n.iter)
  x <- results[[i]]$draws
  vec.3.m <- merge.migrs(vec.3, dicts[[i]])
  
  # Для каждой матрицы мы берем последний столбец с PD и 
  # берем из него PD в соответствии с портфелем. Затем домножаем
  # на константы - LGD, EAD, D - и суммируем. Получаем n.iter значений ECL.
  # Записываем все в вектор значений
  for (j in 1:n.iter) {
    my.ECL <- sum(data.frame(expm::expm(x[[j]]))[vec.3.m, i] * .65 * .85 / 1.1)
    ECL.results[[i]][j] <- my.ECL
  }
  
  # удаляем ненужные переменные
  rm(x, my.ECL, j, vec.3.m)
}



# ECL confidence interval width ==========================
ECL_long <- c()
for (i in c(3, 4, 5, 7, 8, 11, 12, 17, 20, 32)) ECL_long <- c(ECL_long, ECL.results[[i]])
ECL_long <- data.frame(ECL = ECL_long,
                       n_grp = rep(c(3, 4, 5, 7, 8, 11, 12, 17, 20, 32), each = n.iter))
ECL_long[ECL_long$n_grp == 8, ]


# graphs
require(ggplot2)
ggplot(data = ECL_long, aes(ECL, fill = n_grp)) +
  geom_density(alpha = .3) +
  theme_classic()


means <- c()
for (i in c(3, 4, 5, 7, 8, 11, 12, 17, 20, 32)) means <- c(means, (mean(ECL_long[as.numeric(as.character(ECL_long$n_grp)) == i, 1])))
plot(x = c(3, 4, 5, 7, 8, 11, 12, 17, 20, 32),
     y = means, type = "l")

widths <- c()
for (i in c(3, 4, 5, 7, 8, 11, 12, 17, 20, 32)) {
  widths <- c(widths, 
              quantile(ECL_long[as.numeric(as.character(ECL_long$n_grp)) == i, 1], .975) - 
                quantile(ECL_long[as.numeric(as.character(ECL_long$n_grp)) == i, 1], .025)
              )
  }
plot(x = c(3, 4, 5, 7, 8, 11, 12, 17, 20, 32),
     y = widths, type = "b")

