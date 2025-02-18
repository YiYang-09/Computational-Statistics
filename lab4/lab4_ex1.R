# define target function
f <- function(x){
  
  if(x > 0){
    120*(x^5)*exp(-x)
    
  } else {
    stop("x must be positive")
  }
}

# a

MetropolisHastings <- function(x_init){
  set.seed(1234)
  x <- numeric(length = 10000)
  x[1] <- x_init
  
  for(t in 1:9999){
    
    # sample candidate for x* from a Normal(mean = x[t-1], sd = 0.1)
    x_candidate <- rnorm(n = 1, mean = x[t], sd = 0.1)
    
    # calculate acceptance rate
    r <- f(x_candidate) / f(x[t]) # since proposal is symmetric, R = f(x*)/f(x_t)
    
    # sample x[t+1]
    if (runif(1) < min(r, 1)) {
      # Accept new candidate
      x[t+1] <- x_candidate  
      
    } else {
      # Stay at previous value
      x[t+1] <- x[t]  
    }
  }
  
  return(x)
}

x <- MetropolisHastingsNormal(x_init = 1)

# x[t] <- sample(c(x_candidate, x[t-1]), size = 1, prob = c(min(r, 1), 1-min(r, 1)))

plot(x, type = "l")
hist(x)

# a, b, c 

MetropolisHastings <- function(x_init, n_iter, proposal_dist = "Other"){
  set.seed(1234)
  x <- numeric(length = n_iter)
  x[1] <- x_init
  acc <- 1
  
  for(t in 2:(n_iter)){
    
    # sample candidate for x* from a proposal distribution
    if(proposal_dist == "Normal"){
      x_candidate <- rnorm(n = 1, mean = x[t - 1], sd = 0.1)
      
    } else if(proposal_dist == "Chi"){
      x_candidate <- rchisq(n = 1, floor(x[t - 1] + 1))
      
    } else {
      x_candidate <- runif(n = 1, min = x[t - 1] - 0.3, max = x[t - 1] + 0.3)
    }
    
    # if not symmetric
    if(proposal_dist == "Chi"){
      num <- f(x_candidate) * dchisq(x[t - 1], df = floor(x_candidate))
      denom <- f(x[t - 1]) * dchisq(x_candidate, df = floor(x[t - 1]))
      r <- num / denom
    }
    
    # calculate acceptance rate
    r <- f(x_candidate) / f(x[t - 1]) # since proposal is symmetric, R = f(x*)/f(x_t)
    
    # sample x[t+1]
    if (runif(1) < min(r, 1)) {
      # Accept new candidate
      x[t] <- x_candidate  
      acc <- acc + 1
      
    } else {
      # Stay at previous value
      x[t] <- x[t - 1]  
    }
  }
  
  return(list(chain = x, acceptance_rate = sum(acc)/n_iter))
}

x1 <- MetropolisHastings(x_init = 1, n_iter = 10000, proposal_dist = "Normal")$chain
x2 <- MetropolisHastings(x_init = 1, n_iter = 10000, proposal_dist = "Chi")$chain
x3 <- MetropolisHastings(x_init = 1, n_iter = 10000)$chain

acc1 <- MetropolisHastings(x_init = 1, n_iter = 10000, proposal_dist = "Normal")$acceptance_rate
acc2 <- MetropolisHastings(x_init = 1, n_iter = 10000, proposal_dist = "Chi")$acceptance_rate
acc3 <- MetropolisHastings(x_init = 1, n_iter = 10000)$acceptance_rate

par(mfrow = c(1, 3))
plot(x1, type = "l")
plot(x2, type = "l")
plot(x3, type = "l")

hist(x1)
hist(x2)
hist(x3)

# e
mean(x1)
mean(x2)
mean(x3)

# f
# f ~ Gamma(a = 6, b = 1) -> E(X) = 6*1 = 6




#####################################################################################################

MetropolisHastings <- function(x_init, n_iter, proposal_dist = "Other"){
  set.seed(1234)
  x <- numeric(length = n_iter)
  x[1] <- x_init
  acc <- 0
  
  for(t in 2:(n_iter)){
    
    # sample candidate for x* from a proposal distribution
    if(proposal_dist == "Normal"){
      x_candidate <- rnorm(n = 1, mean = x[t - 1], sd = 0.1)
      
    } else if(proposal_dist == "Chi"){
      x_candidate <- rchisq(n = 1, floor(x[t - 1] + 1))
      
    } else {
      x_candidate <- runif(n = 1, min = x[t - 1] - 0.5, max = x[t - 1] + 0.5)
    }
    
    # if asymmetric distribution
    if(proposal_dist == "Chi"){ 
      num <- f(x_candidate) * dchisq(x[t - 1], df = floor(x_candidate))
      denom <- f(x[t - 1]) * dchisq(x_candidate, df = floor(x[t - 1]))
      r <- num / denom
    } else{
      # calculate acceptance rate
      r <- f(x_candidate) / f(x[t - 1]) # since proposal is symmetric, R = f(x*)/f(x_t)
    }
    
    # sample x[t+1]
    if (runif(1) < min(r, 1)) {
      # Accept new candidate
      x[t] <- x_candidate  
      acc <- acc + 1
      
    } else {
      # Stay at previous value
      x[t] <- x[t - 1]  
    }
  }
  
  return(list(chain = x, acceptance_rate = sum(acc)/n_iter))
}

x1 <- MetropolisHastings(x_init = 1, n_iter = 10000, proposal_dist = "Normal")$chain
x2 <- MetropolisHastings(x_init = 1, n_iter = 10000, proposal_dist = "Chi")$chain
x3 <- MetropolisHastings(x_init = 1, n_iter = 10000)$chain

acc1 <- MetropolisHastings(x_init = 1, n_iter = 10000, proposal_dist = "Normal")$acceptance_rate
acc2 <- MetropolisHastings(x_init = 1, n_iter = 10000, proposal_dist = "Chi")$acceptance_rate
acc3 <- MetropolisHastings(x_init = 1, n_iter = 10000)$acceptance_rate

data <- data.frame(it = 1:10000, x1 = x1, x2 = x2, x3 = x3)

ggplot(data, aes(x = it, y = x1)) + 
  geom_line(col = "#4E79A7") + 
  theme_minimal() + 
  labs(x = "Iteration", y = "Chain values")

ggplot(data, aes(x = x1)) + 
  geom_histogram(color = "#BAB0AC", fill = "#4E79A7") + 
  theme_minimal() + 
  labs(x = "Sample values", y = "Count")

ggplot(data, aes(x = x1)) + 
  geom_histogram(color = "#BAB0AC", fill = "#4E79A7", bins = 30) + 
  geom_line(data = data_f, aes(x = x, y = y), color = "black", linewidth = 1) + 
  theme_minimal() + 
  labs(x = "Sample values", y = "Density / f(x)")
