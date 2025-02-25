# f <- function(x, mu, c){
#   exp(-exp(-(x - mu - c)))
# }

f_inv <- function(u, mu, c){
  mu + c - log(-log(u))
}

n <- 13
mu <- 0
c <- log(log(2))

generate_gumbel <- function(mu, n, c){
  U <- runif(n)
  X <- f_inv(U, mu, c)
  return(X)
}


sign_test <- function(X, alpha){
  positives <- sum(X > 0)
  test <- binom.test(positives, length(X), p = 0.5, alternative = "greater")
  return(test$p.value < alpha) # rejection 
}

nreps <- 1000
alpha <- 0.05
mu_vals <- seq(from = 0, to = 2, by = 0.1)
power <- numeric(length(mu_vals))

for(mu in 1:length(mu_vals)){
  count <- 0
  for(i in 1:nreps){
    X <- generate_gumbel(mu_vals[mu], n, c)
    test <- sign_test(X, alpha)
    count <- count + test
  }
  power[mu] <- count/nreps
}

plot(mu_vals, power)
plot_data <- data.frame(mu = mu_vals, power = power)
library(ggplot2)
ggplot(plot_data, aes(x = mu, y = power)) + 
  geom_line(col = "#4E79A7", linewidth = 1) + 
  theme_minimal() + 
  labs(x = expression(mu), y = "Power of the sign test")


# power <- sapply(mu_vals, function(mu) {
#   rejections <- replicate(nreps, {
#     X <- generate_gumbel(mu, n, c)
#     sign_test(X, alpha)
#   })
#   mean(rejections)  # Proportion of rejections = Power
# })

for (j in seq_along(mu_vals)) {
  results <- replicate(nreps, {
    X <- generate_gumbel(mu_vals[j], n, c)
    sign_test(X, alpha)
  })
  power[j] <- mean(results)
}


# library(ordinal)
# X <- qgumbel(U, location = 1, scale = mu + c)
# plot(X, type = "l")
