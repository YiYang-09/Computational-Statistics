library(ggplot2)
#a draw the boundary 
w  <- 1.999
x_max <- sqrt(4 / (4 - w^2))  # maximum of x1
xv <- seq(-x_max, x_max, by=0.01)  # x1


x2_pos <- -(w/2)*xv + sqrt(1 - (1 - w^2/4) * xv^2)
x2_neg <- -(w/2)*xv - sqrt(1 - (1 - w^2/4) * xv^2)


ellipse_df <- data.frame(
  x1=c(xv,rev(xv)),
  x2=c(x2_pos,rev(x2_neg))
)

ggplot(ellipse_df,aes(x=x1,y=x2))+
  geom_path(color="#4E79A7",size=1)+
  labs(x=expression(x[1]),y=expression(x[2]))+
  theme_minimal()


#b

#c Gibbs sampling

gibbsSampling <- function(n,W){
  
  #initialize x1 and x2
  X1 <- numeric(n)
  X2 <- numeric(n)
  
  #initialize the first iteration
  X1[1] <- runif(1,-1,1)
  X2[1] <- runif(1,-1,1)
  
  for(i in 2:n){
   #generating x1 given x2
   X1_range <- c(-0.5*W*X2[i-1]-sqrt(1-(1-1/4*W^2)*X2[i-1]^2),-0.5*W*X2[i-1]+sqrt(1-(1-1/4*W^2)*X2[i-1]^2))
   X1[i] <- runif(1,min = X1_range[1],max = X1_range[2]) 
    
   #generating x2 given x1
   X2_range <- c(-0.5*W*X1[i]-sqrt(1-(1-1/4*W^2)*X1[i]^2),-0.5*W*X1[i]+sqrt(1-(1-1/4*W^2)*X1[i]^2))
   X2[i] <- runif(1,min = X2_range[1],max = X2_range[2])
    
  }
   result <- data.frame(X1,X2)
   return(result)
}
set.seed(12345)
samples <- gibbsSampling(1000,1.999)
ggplot(ellipse_df,aes(x=x1,y=x2))+
  geom_path(color="#4E79A7",size=1)+
  labs(x=expression(x[1]),y=expression(x[2]))+
  geom_point(data=samples,aes(x = X1, y = X2), color = "red", size = 0.5) +
  theme_minimal()


#repeat the sampling 
prob_x1 <- function(sample,n){
  prob <- sum(sample$X1>0)/n
  return(prob)
  
}

probs <- numeric(10)
sample_result <- list()
for (i in 1:10) {
  sample_result[[i]] <- gibbsSampling(1000,1.999)
  probs[i] <- prob_x1(sample_result[[i]],1000)
}
cat("The probabilities of X1 >0 in 10 times :",probs,"\n")


#d

#e generate U


#calculate U1 = X1 - X2, U2 = X1 + X2
U1<- c(xv - x2_pos, rev(xv - x2_neg))
U2 <- c(xv + x2_pos, rev(xv + x2_neg))

#plot the boundaries of U
ellipse_U_df <- data.frame(U1 = U1, U2 = U2)
ggplot(ellipse_U_df, aes(x = U1, y = U2)) +
  geom_path(color = "#4E79A7", size = 1) +  
  labs(x = expression(U[1]), y = expression(U[2])) +
  theme_minimal()

#generate 1000 random variables using Gibbs Sampling
gibbsSampling_U <- function(n, W) {
  # initialize U1 and U2 
  U1 <- numeric(n)
  U2 <- numeric(n)
  
  # initialize the first iteration
  U1[1] <- 0  
  U2[1] <- 0
  
  for (i in 2:n) {
    # given U2[i-1]，generate U1[i]
    numerator_U1 <- 4 - (2 + W) * U2[i-1]^2
    U1_max <- sqrt(numerator_U1 / (2 - W))
    U1_range <- c(-U1_max, U1_max)
    
    U1[i] <- runif(1, min = U1_range[1], max = U1_range[2])
    
    # given U1[i]，generate U2[i]
    numerator_U2 <- 4 - (2 - W) * U1[i]^2
    U2_max <- sqrt(numerator_U2 / (2 + W))
    U2_range <- c(-U2_max, U2_max)
    U2[i] <- runif(1, min = U2_range[1], max = U2_range[2])
  }
  data.frame(U1, U2)
}
set.seed(12345)
sample_U <- gibbsSampling_U(1000,1.999)

#plot the samples of U
ggplot(ellipse_U_df, aes(x = U1, y = U2)) +
  geom_path(color = "#4E79A7", size = 1) +  
  labs(x = expression(U[1]), y = expression(U[2])) +
  geom_point(data=sample_U,aes(x = U1, y = U2), color = "red", size = 1) +
  theme_minimal()

#calculate P(x1>0)
samples_U_transformed_df <- data.frame(
  X1=(sample_U$U1+sample_U$U2)/2,
  X2=(sample_U$U2-sample_U$U1)/2
)

prob_X1_gibbs <- sum(samples_U_transformed_df$X1 > 0) /1000

cat("The probability of X1 > 0:",prob_X1_gibbs,"\n")


#repeat the sampling
prob_U <- function(sample,n){
  prob <- sum((sample$U2 + sample$U1) / 2 > 0) / 1000
  return(prob)
  
}
set.seed(12345)
probs_u <- numeric(10)
sample_U_result <- list()
for (i in 1:10) {
  sample_U_result[[i]] <- gibbsSampling_U(1000,1.999)
  probs_u[i] <- prob_U (sample_U_result[[i]],1000)
}
kable(probs_u,caption = "Probability of X1>0")




