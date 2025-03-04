library(ggplot2)

# algorithm

em <- function(data, eps = 0.0001){
  n      <- length(data)
  pi     <- matrix(NA, ncol = 3, nrow = n)  
  
  # Random initialization of the parameters
  p <- rep(1/3, 3)
  sigma <- rep(sd(data)*2/3, 3)
  mu <- c(mean(data) - sigma[1]/2, mean(data), mean(data) + sigma[1]/2)
  pv <- c(p, mu, sigma)
  cc  <- eps + 100
  mu_list <- c(mu)
  sigma_list <- c(sigma)
  
  while (cc > eps){
    # save previous parameter vector
    pv1  <- pv                 
    
    # E step 
    for (j in 1:n){
      pi1 <- p[1]*dnorm(data[j], mean=mu[1], sd=sigma[1])
      pi2 <- p[2]*dnorm(data[j], mean=mu[2], sd=sigma[2])
      pi3 <- p[3]*dnorm(data[j], mean=mu[3], sd=sigma[3])
      
      total <- pi1+pi2+pi3
      pi[j,] <- c(pi1/total, pi2/total, pi3/total)
    }

    # M step #
    p <- colSums(pi)/n
    mu[1] <- sum(pi[, 1]*data)/(p[1]*n)
    mu[2] <- sum(pi[, 2]*data)/(p[2]*n)
    mu[3] <- sum(pi[, 3]*data)/(p[3]*n)
    sigma[1] <- sqrt(sum(pi[, 1]*((data-mu[1])^2))/(p[1]*n))
    sigma[2] <- sqrt(sum(pi[, 2]*((data-mu[2])^2))/(p[2]*n))
    sigma[3] <- sqrt(sum(pi[, 3]*((data-mu[3])^2))/(p[3]*n))
    
    # stopping criteria
    pv <- c(p, mu, sigma)
    cc     <- sum((pv - pv1)^2) / sum(pv1^2)  # a convergence criterion, maybe not the best one
    mu_list <- rbind(mu_list, mu)
    sigma_list <- rbind(sigma_list, sigma)
    
  }
  res <- list(final_probs = pv[1:3], 
              final_mus = pv[4:6],
              final_sigmas = pv[7:9],
              mu = mu_list, 
              sigma = sigma_list)
  return(res)
}

# run algorithm with data

load(file = "D:\\liu\\CompStat\\Computational-Statistics\\lab6\\threepops.Rdata")
data <- dat3p
eps <- 0.000001

res <- em(data, eps)

# histogram of data

df <- data.frame(data)
ggplot(df, aes(x = df)) + 
  geom_histogram(col = "black", fill = "#4E79A7") + 
  labs(title = "Histogram of dat3p", x = "Value", y = "Frequency") +
  theme_minimal()


#  plots of current estimates for each model parameter versus the iteration-number

mus <- data.frame(res$mu)
sigmas <- data.frame(res$sigma)

# mu plot
ggplot(data = mus, aes(x = 1:nrow(mus))) + 
  geom_line(aes(y = mus[, 1]), col = "#4E79A7", size = 0.7) + 
  geom_line(aes(y = mus[, 2]), col = "#F28E2B", size = 0.7) + 
  geom_line(aes(y = mus[, 3]), col = "#E15759", size = 0.7) + 
  theme_minimal() + 
  labs(x = "Iterations", y = expression(mu))

# sigma plot
ggplot(data = sigmas, aes(x = 1:nrow(sigmas))) + 
  geom_line(aes(y = sigmas[, 1]), col = "#4E79A7", size = 0.7) + 
  geom_line(aes(y = sigmas[, 2]), col = "#F28E2B", size = 0.7) + 
  geom_line(aes(y = sigmas[, 3]), col = "#E15759", size = 0.7) + 
  theme_minimal() + 
  labs(x = "Iterations", y = expression(sigma))


#--------------------------------------------------------------------------------------------
# visualize final mixture distribution

# Generate random values based on the mixture probabilities
set.seed(123) 

# Sample the components based on the probabilities
n <- 1000  
samples <- sample(1:3, size = n, replace = TRUE, prob = res$final_probs)

# Generate the data according to the chosen components
distrib1 <- rnorm(sum(samples == 1), mean = res$final_mus[1], sd = res$final_sigmas[1])
distrib2 <- rnorm(sum(samples == 2), mean = res$final_mus[2], sd = res$final_sigmas[2])
distrib3 <- rnorm(sum(samples == 3), mean = res$final_mus[3], sd = res$final_sigmas[3])

# Combine the generated data
values <- c(distrib1, distrib2, distrib3)

# Create a data frame
data <- data.frame(value = values)

# Plot the mixture distribution
ggplot(data, aes(x = value)) +
  geom_density(fill = "#4E79A7", alpha = 0.5) +  # Basic density plot with fill color
  labs(title = "Mixture Distribution", x = "Value", y = "Density") +
  theme_minimal()
