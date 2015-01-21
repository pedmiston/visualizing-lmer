library(MASS)

set.seed(723)
num_subjects <- 24


intercepts <- rnorm(num_subjects, mean = 300, sd = 40)
slopes <- rnorm(num_subjects, mean = 10, sd = 10)

mvrnorm

shuffle_until <- function(x, y, lwr, upr, max_iter = 100000, seed = NULL) {
  set.seed(seed)
  i <- 1
  repeat {
    current <- cor(x, y)
    if (current > lwr & current <= upr) {
      break
    } else if (i > max_iter) {
      warning("Maximum iteration reached"); break
    }
    y <- sample(y)
    i <- i + 1
  }
  data.frame(x, y)
}

correlated <- shuffle_until(intercepts, slopes, lwr = 0.8, upr = 1.0)
cor(correlated$x, correlated$y)