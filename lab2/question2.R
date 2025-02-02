#a

#log likelihood
g <- function(b){
  xi <- c(0,0,0,0.1,0.1,0.3,0.3,0.9,0.9,0.9)
  yi <- c(0,0,1,0,1,1,1,0,1,1)
  p <- 1/(1+exp(-b[1]-b[2]*xi))   #b[1] represents b0,b2 represents b1
  loglik <- sum(yi*log(p)+(1-yi)*log(1-p))
  return(loglik)
}

#gradient
dg <- function(b){
  xi <- c(0,0,0,0.1,0.1,0.3,0.3,0.9,0.9,0.9)
  yi <- c(0,0,1,0,1,1,1,0,1,1)
  p <- 1/(1+exp(-b[1]-b[2]*xi))
  
  dg_b0 <- sum(yi-p) #partial derivative with respect to b0
  dg_b1 <- sum(xi*(yi-p)) #partial derivative with respect to b1
  return(c(dg_b0,dg_b1))
}

#produce the contours plot
x1grid <- seq(-0.5, 1, by=0.05) #x轴点数
x2grid <- seq(-1, 3, by=0.05) #y轴点数
dx1 <- length(x1grid)
dx2 <- length(x2grid)
dx  <- dx1*dx2 #总点数
gx  <- matrix(rep(NA, dx), nrow=dx1)
for (i in 1:dx1)
  for (j in 1:dx2)
  {
    gx[i,j] <- g(c(x1grid[i], x2grid[j]))  
  }
mgx <- matrix(gx, nrow=dx1, ncol=dx2)
contour(x1grid, x2grid, mgx, nlevels=30)  # Note: For other functions g, you might need to choose another nlevels-value to get a good impression of the function 

#Steepest ascent function:
steepestasc <- function(x0, eps=1e-8, alpha0=1,path_col=2, final_col=4)
{
  xt   <- x0
  conv <- 999
  
  #function and gradient evaluations
  funEvaluation <- 0
  gradEvaluation <- 0
  
  points(xt[1], xt[2], col=path_col, pch=4, lwd=3)
  
  
  while(conv>eps)
  {
    alpha <- alpha0
    xt1   <- xt
    grad_xt1 <- dg(xt1)
    xt    <- xt1 + alpha*grad_xt1
    
    #update gradient evaluation
    gradEvaluation <-gradEvaluation+1
    
    #update function evaluation
    g_x <- g(xt)
    funEvaluation <-funEvaluation+1
    g_x1 <- g(xt1)
    funEvaluation <-funEvaluation+1
    
    while (g_x<g_x1 )
    {
      alpha <- alpha/2
      xt    <- xt1 + alpha*grad_xt1
      
      g_x <- g(xt)
      funEvaluation <-funEvaluation+1
    }
    points(xt[1], xt[2],col=path_col,pch=4, lwd=1)
    conv <- sum((xt-xt1)*(xt-xt1))
  }
  points(xt[1], xt[2], col=final_col ,pch=4, lwd=3)
  return(list(coefficients=xt,
              
              funEvaluation=funEvaluation,
              gradientEvaluation=gradEvaluation))
}

#b

inital_b <- c(-0.2,1)

#the second solution: a=a

steepestasc_keep <- function(x0, eps=1e-5, alpha0=1, path_col=3, final_col=6){
  
  xt   <- x0
  conv <- 999
  
  #function and gradient evaluations
  funEvaluation <- 0
  gradEvaluation <- 0
  
  points(xt[1], xt[2], col=path_col, pch=4, lwd=3)
  
  
  alpha <- alpha0 
  
  while(conv>eps)
  {
    
    xt1   <- xt
    grad_xt1 <- dg(xt1) 
    xt    <- xt1 + alpha*grad_xt1
    
    #update gradient evaluation
    gradEvaluation <-gradEvaluation+1
    
    #update function evaluation
    g_x <- g(xt)
    funEvaluation <-funEvaluation+1
    g_x1 <- g(xt1)
    funEvaluation <-funEvaluation+1
    
    while (g(xt)<g(xt1))
    {
      alpha <- alpha/2
      xt    <- xt1 + alpha*grad_xt1
      
      g_x <- g(xt)
      funEvaluation <- funEvaluation + 1 
    }  
    points(xt[1], xt[2],col=path_col,  pch=4, lwd=1)
    conv <- sum((xt-xt1)*(xt-xt1))
  }
  points(xt[1], xt[2], col=final_col, pch=4, lwd=3)
  return(list(coefficients=xt,
              funEvaluation=funEvaluation,
              gradientEvaluation=gradEvaluation))
  
  
}

#first solution: a=a0

print(steepestasc(inital_b,eps=1e-5, alpha0=1))
#second solution: 

print(steepestasc_keep(inital_b,eps=1e-5, alpha0=1))



#c

# BFGS 
optim_BFGS <- optim(par = inital_b,fn=g,gr=dg,method = "BFGS",control = list(fnscale = -1))
print(optim_BFGS)

#Nelder-Mead


optim_NM <- optim(par = inital_b,fn=g,gr=dg,method ="Nelder-Mead",control = list(fnscale = -1))
print(optim_NM)



#d.glm
xi <- c(0,0,0,0.1,0.1,0.3,0.3,0.9,0.9,0.9)
yi <- c(0,0,1,0,1,1,1,0,1,1)
fit <- glm(yi~xi,family = binomial)
cat("The coefficients are:","\n",coef(fit))
