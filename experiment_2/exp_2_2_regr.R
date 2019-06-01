# Создадим Array следующего вида:
# 1 и 2 измерения соответствуют коэффициентам матрицы переходных вероятностей
# 3 измерение соответствует длину выборки. Длина выборки определяется как my.Ns[i],
# где i - номер по 3ему измерению
load("exp_2_BMCMC.RData")
source("../functions.R")

# ширина доверительного интервала в долях (при домножении на 100 получаем пп)
MCMC.int <- MCMC.m[, , 3, ] - MCMC.m[, , 1, ]

# количество переходов (наблюдений) из рейтинга в рейтинг
num.migr <- array(NA, c(5, 5, 2491))
for (i in 1:2491) {
  num.migr[,,i] <- get.m(rs[[i]][,1], rs[[i]][,2], 5)
}


## Experiment 2.1 ====

build.graph <- function(MCMC.int, 
                        num.migr,
                        fun = function(x)1/(x^2),
                        from = 1, 
                        to = 1,
                        label = "") {
  # df, куда сохраняются пары данных
  # количество миграций - ширина дов интервала
  df <- data.frame(
    `Interval Width` <- fun(MCMC.int[from, to, ]),
    `No of migrations` <- num.migr[from, to, ]
  )
  ggplot(data = df, aes(x = `No of migrations`, y = `Interval Width`)) +
    geom_point(color = beau_palette[1],
               size = .1) +
    #  geom_line(aes(x = 5:400, y = y.pred), color = "red", size = 1) +
    beau_theme +
    xlab(paste("Количество миграций", from, "-", to)) +
    ylab(paste("Ширина доверительного\nинтервала", label, sep = ""))
}

# Graph 6
build.graph(MCMC.int, 
            num.migr,
            fun = function(x)x,
            from = 1, 
            to = 1,
            label = "")

# Graph 7
build.graph(MCMC.int, 
            num.migr,
            fun = function(x)x,
            from = 1, 
            to = 2,
            label = "")

# Graph 8
build.graph(MCMC.int, 
            num.migr,
            fun = function(x)1/(x^2),
            from = 1, 
            to = 1,
            label = "")

# Graph 9
build.graph(MCMC.int, 
            num.migr,
            fun = function(x)1/(x^2),
            from = 1, 
            to = 2,
            label = "")







## Experiment 2.2 ====
build.mdl <- function(MCMC.int, 
                      num.migr,
                      fun = function(x) 1/(x^2),
                      from = 1, 
                      to = 1,
                      type = "WLS",
                      intercept = FALSE) {
  df <- data.frame(
    x <- fun(MCMC.int[from, to, ]),
    y <- num.migr[from, to, ]
  )
  names(df) <- c("h", "migr")
  
  if (intercept) {
    if (type == "WLS") {
      mdl <- lm(data = df, migr ~ h, weights = h)
      print(summary(mdl))
      return(mdl)
    } else if (type == "OLS") {
      mdl <- lm(data = df, migr ~ h)
      print(summary(mdl))
      return(mdl)
    } else {
      print("Regression type is not recognized. Please choose either OLS or WLS!")
    }
  } else {
    if (type == "WLS") {
      mdl <- lm(data = df, migr ~ h -1, weights = h)
      print(summary(mdl))
      return(mdl)
    } else if (type == "OLS") {
      mdl <- lm(data = df -1, migr ~ h)
      print(summary(mdl))
      return(mdl)
    } else {
      print("Regression type is not recognized. Please choose either OLS or WLS!")
    }
  }
}




## Составление таблицы ====

## Results With intercept - Table 13
# Example
build.mdl(MCMC.int, 
          num.migr,
          fun = function(x) 1/(x^2),
          from = 1, 
          to = 1,
          type = "WLS",
          intercept = FALSE)

froms = c(1,1,2,2,2,3,3,3,4,4,4)
tos   = c(1,2,1,2,3,2,3,4,3,4,5)
df.res <- data.frame(matrix(rep(NA, 55), c(11, 5)))
names(df.res) <- c("from", "to", "p", "beta_0", "beta_1")
for (i in 1:11) {
  df.res$from[i] <- froms[i]
  df.res$to[i] <-   tos[i]
  df.res$p[i] <-    trans.m[froms[i], tos[i]]
  mdl <- build.mdl(MCMC.int, 
                   num.migr,
                   fun = function(x) 1/(x^2),
                   from = froms[i], 
                   to = tos[i],
                   type = "WLS",
                   intercept = T)
  df.res$beta_0[i] <- mdl$coefficients[[1]]
  df.res$beta_1[i] <- mdl$coefficients[[2]]
}
print("Results with intercept - Table 13")
print(df.res)




## Results Without intercept 
froms = c(1,1,2,2,2,3,3,3,4,4,4)
tos   = c(1,2,1,2,3,2,3,4,3,4,5)
df.res.1 <- data.frame(matrix(rep(NA, 44), c(11, 4)))
names(df.res.1) <- c("from", "to", "p", "beta_1")
for (i in 1:11) {
  df.res.1$from[i] <- froms[i]
  df.res.1$to[i] <-   tos[i]
  df.res.1$p[i] <-    trans.m[froms[i], tos[i]]
  mdl <- build.mdl(MCMC.int, 
                   num.migr,
                   fun = function(x) 1/(x^2),
                   from = froms[i], 
                   to = tos[i],
                   type = "WLS",
                   intercept = F)
  df.res.1$beta_1[i] <- mdl$coefficients[[1]]
}
print("Results without intercept")
print(df.res.1)