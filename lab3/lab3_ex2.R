# a


n <- 10000000
mu <- c(0, 0)
sigma <- matrix(c(0.6, 0, 0, 0.6), nrow = 2)

box_muller_normal <- function(n, mu, sigma){

  set.seed(1234)
  u <- runif(n)
  v <- runif(n)
  
  x1 <- sqrt(-2*log(u))*cos(2*pi*v)*sqrt(sigma[1, 1]) + mu[1]
  x2 <- sqrt(-2*log(u))*sin(2*pi*v)*sqrt(sigma[2, 2]) + mu[2]
  
  proc.time()

  return(c(x1, x2))
}

dist <- box_muller_normal(n, mu, sigma)
proc.time()

plot(density(dist))


# b

library(mvtnorm)

set.seed(1234)
dist2 <- rmvnorm(n, mean = mu, sigma = sigma)
proc.time()

plot(density(dist2))


# c

mu1 <- c(0, 0)
mu2 <- c(1.2, 1.5)

sigma1 <- matrix(c(0.6, 0, 0, 0.6), nrow = 2)
sigma2 <- matrix(c(0.5, 0, 0, 0.5), nrow = 2)

prob <- c(0.5, 0.5)
n <- 1000


g <- sample(length(mu), n, replace=TRUE, p=prob)
x <- rnorm(n, mean = mu[g], sd = sigma[g])
