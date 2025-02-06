##question1
#a
library(ggplot2)
fx <- function(x){
  if(x< -1|x > 1){
    return(0)}
  if(-1 <= x&& x <= 0){
    return(x+1)}
  if(0 < x && x <= 1){
    return(1-x)}
}

rejection_sample <- function(n){
  samples <- numeric(n)
  count <- 0
  
  while (count<n) {
    sample <- runif(1,-1,1) #sampling from uniform distribution
    U <- runif(1)
    f_x <- fx(sample)
    
    if(U <= f_x){
      count <- count+1
      samples[count] <- sample
    }
    
  }
  return(samples)
  
}
set.seed(12345)
x <- rejection_sample(10000)
#hist(x,breaks = 100)
data <- data.frame(x)
ggplot(data,aes(x=x))+
  geom_histogram(bins = 100,fill="#4E79A7",color="black")+
  labs(title = "Histogram of x using  rejection sampling", x = "x", y ="Frequency")+ theme_minimal()




#b
com_sample <- function(n){
  U1 <- runif(n/2) #50%
  Y1 <- 1-sqrt(1-U1)
  
  U2 <- runif(n/2)
  Y2 <- -(1-sqrt(1-U2))
  
  X <- c(Y1,Y2)
  return(X)
  
}
set.seed(12345)
composition_samples <- com_sample(10000)
#hist(composition_samples,breaks = 100)
dataCS <- data.frame(composition_samples)
ggplot(dataCS,aes(x=x))+
  geom_histogram(bins = 100,fill="#4E79A7",color="black")+
  labs(title = "Histogram of x using composition sampling", x = "x", y ="Frequency")+ theme_minimal()

#c.
set.seed(12345)
U1 <- runif(10000,0,1)
U2 <- runif(10000,0,1)

U3 <- U1-U2
#hist(U3,breaks = 100)
dataDifference <- data.frame(U3)
ggplot(dataDifference,aes(x=x))+
  geom_histogram(bins = 100,fill="#4E79A7",color="black")+
  labs(title = "Histogram of x using the difference of uniform distributions", x = "x", y ="Frequency")+ theme_minimal()
#d
cat("Variance of X: ",var(U3),"\n")