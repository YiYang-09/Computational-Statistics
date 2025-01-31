# a
data <- data.frame(x = c(0, 0, 0, 0.1, 0.1, 0.3, 0.3, 0.9, 0.9),
                   y = c(0, 0, 1, 1, 1, 1, 0, 1, 1))
x = c(0, 0, 0, 0.1, 0.1, 0.3, 0.3, 0.9, 0.9)
y = c(0, 0, 1, 1, 1, 1, 0, 1, 1)

logreg <- function(b, x){
  denom <- 1 + exp(-b[1] - b[2]*x)
  return(1/denom)
}

loglik <- function(b){
  p <- logreg(b, x)
  return(sum(y * log(p) + (1 - y) * log(1 - p)))
}

gradient <- function(b) {
  p <- logreg(b, x)
  grad_b0 <- sum(y - p)
  grad_b1 <- sum((y - p) * x)
  return(c(grad_b0, grad_b1))
}

# Steepest ascent function
steepestasc <- function(x0, eps = 1e-5, alpha0 = 1) {
  xt <- x0
  conv <- 999
  func_eval <- 0
  grad_eval <- 0
  
  while (conv > eps) {
    alpha <- alpha0
    xt1 <- xt
    grad <- gradient(xt1)
    grad_eval <- grad_eval + 1
    
    xt <- xt1 + alpha * grad
    func_eval <- func_eval + 1
    
    while (loglik(xt) < loglik(xt1)) {
      alpha <- alpha / 2
      xt <- xt1 + alpha * grad
      func_eval <- func_eval + 1
    }
    
    conv <- sum((xt - xt1)^2)
  }
  
  list(estimate = xt, func_evals = func_eval, grad_evals = grad_eval)
}

# b
x0 <- c(-0.2, 1)
steepestasc(x0)
steepestasc(x0, alpha = 0.5)

# d
mod <- glm(y ~ x, family=binomial(link='logit'))
summary(mod)
