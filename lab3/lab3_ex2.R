library(ggplot2)

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

  return(data.frame(x1, x2))
}

start_time <- proc.time()
dist <- box_muller_normal(n, mu, sigma)
end_time <- proc.time()

print(end_time - start_time)

system.time({
  dist <- box_muller_normal(n, mu, sigma)
})


# ggplot(dist, aes(x = x1, y = x2)) +
#   geom_point(alpha = 0.2) +
#   theme_minimal() +
#   ggtitle("Bivariate Normal Distribution") +
#   xlab("X1") + 
#   ylab("X2")


# b

library(mvtnorm)

set.seed(1234)
dist2 <- rmvnorm(n, mean = mu, sigma = sigma)

system.time({
  dist2 <- rmvnorm(n, mean = mu, sigma = sigma)
})

# plot(density(dist2))


# c

mu1 <- c(0, 0)
mu2 <- c(1.2, 1.5)
mu <- list(mu1, mu2)

sigma1 <- matrix(c(0.6, 0, 0, 0.6), nrow = 2)
sigma2 <- matrix(c(0.5, 0, 0, 0.5), nrow = 2)
sigma <- list(sigma1, sigma2)

prob <- c(0.5, 0.5)
n <- 1000

x <- data.frame(matrix(NA, nrow = n, ncol = 3))
set.seed(1234)
for(i in 1:n){
  g <- sample(1:2, 1, replace = TRUE, p = prob)
  x[i, 1:2] <- rmvnorm(n = 1, mean = mu[[g]], sigma = sigma[[g]])
  x[i, 3] <- as.character(g)
}

ggplot(x, aes(x = x[, 1], y = x[, 2], color = x[, 3])) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  scale_color_manual(values = c("#4E79A7", "#E15759")) + 
  labs(title = "Bivariate Normal Mixture", x = "X1", y = "X2", color = "Original distribution") + 
  theme(legend.position="bottom")
