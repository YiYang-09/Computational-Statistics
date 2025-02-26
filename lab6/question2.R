#question2
library(ggplot2)
load("bankdata.Rdata")
data <- as.data.frame(bankdata)
nclients <- dim(data)[1]  # number of individuals in the dataset, here 4364


crit <- function(dat, subs){
  s <- length(subs)
  dist <- matrix(rep(NA, nclients*s), ncol=s)
  for (i in 1:s){
    dist[, i] <- sqrt((dat[,1]-dat[subs[i],1])^2+(dat[,2]-dat[subs[i],2])^2)
  }
  sum(apply(dist, 1, min))
}


#a
set.seed(12345)
initial <- sample(1:nclients,22)
df <- data.frame(age=data$age,balance=data$balance)
ggplot(df,aes(age,balance))+
  geom_point(color="#4E79A7",size=2,alpha=0.6)+
  geom_point(data=df[initial,],aes(age,balance),color="red",size=2)+
  theme_minimal()

#b simulated annealing 



exchange_clients <- function(initial,nclients){
  
  remove_index <- sample(1:length(initial),1)
  #select one sample from dataset
  add_index <- sample(setdiff(1:nclients,initial),1)
  new_initial <- initial
  new_initial[remove_index] <- add_index
  return(new_initial)
  
}

simulated_annealing <- function(data,initial,max_stage,initial_temperature,alpha,initial_iteration,beta){
  
  current_subsample <- initial
  current_crit <- crit(data,current_subsample)
  best_subsample <- current_subsample
  best_crit <- current_crit
  
  temperature <- initial_temperature
  iter_per_stage <- initial_iteration
  crit_values <- numeric()
  
  for(stage in 1:max_stage){
    
    for (i in 1:iter_per_stage) {
      
      #generate new subsample
      new_subsample <- current_subsample
      remove_index <- sample(1: length(current_subsample),1)
      #select one sample from dataset
      add_index <- sample(setdiff(1:nclients,current_subsample),1)
      new_subsample[remove_index] <- add_index
      
      #calculate new crit
      new_crit <- crit(data,new_subsample)
      
      #calculate the acceptance probability
      differernce_crit <- new_crit- current_crit
      accept_probability <- ifelse(differernce_crit<0,1,exp(-differernce_crit/temperature))
      
      if(runif(1)<accept_probability){
        current_subsample <- new_subsample
        current_crit <- new_crit
        
      }
      
      if(current_crit<best_crit){
        best_subsample <- current_subsample
        best_crit <- current_crit
      }
    
      crit_values[i] <- current_crit
    }
    
      temperature <- temperature*alpha
      iter_per_stage <- iter_per_stage*beta
      

  }
  
  return(list(
    best_subsample=best_subsample,
    best_crit=best_crit,
    crit_values=crit_values
  ))
  
  
}



result1 <- simulated_annealing(data=data,initial=initial,max_stage=10,initial_temperature=100,alpha=0.9,initial_iteration=10,beta=1.1)
plot(result1$crit_values,type = "l",col="red",xlab="Iteration Number",ylab="Criterition Value",main="Criterion-Value versus Iteration")
df1 <- data[result1$best_subsample, ]
ggplot(df,aes(age,balance))+
  geom_point(color="#4E79A7",size=2,alpha=0.6)+
  #geom_point(data=df[initial,],aes(age,balance),color="red",size=2)+
  geom_point(data=df1,aes(age,balance),color="blue",size=2)+
  theme_minimal()
