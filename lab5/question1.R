library(ggplot2)
library(boot)
data <- read.csv("kresseertrag.dat",header=FALSE,sep = "")
colnames(data) <- c("Number","Fertilizer","Yield")
data <- as.data.frame(data)

#a
modelA <- lm(Yield~poly(Fertilizer,3,raw = TRUE),data = data)
summary(modelA)

confint(modelA,level = 0.95)


#b
#remove a term
modelB <- lm(Yield~poly(Fertilizer,2,raw = TRUE),data = data)
summary(modelB)

#coefficients with 95% confidence interval
confint(modelB,level = 0.95)

#plot
#plot(data$Fertilizer,data$Yield,main="Yields vs Concentration",
 #    xlab = "Concentration",ylab = "Yields")
#lines(data$Fertilizer,fitted(modelB),col="blue")

ggplot(data,aes(x=Fertilizer,y=Yield))+
  geom_point()+
  labs(title = "Yields vs Concentration",x="Concentration",y="Yields")+
  geom_line(aes(y=fitted(modelB)),color="#4E79A7")
  #geom_line(aes(y=fitted(modelA)),color="red")

#c bootstrap for beta
bo <- 10000                         #bootstrasp replicates
bs <- c()
set.seed(12345)
#save the results
for (i in 1:bo) {
  #sampling using indcies
  indices <- sample(1:nrow(data),size=nrow(data),replace = TRUE)
  bootstrapData <- data[indices,]
  model_bootstrap <- lm(Yield~poly(Fertilizer,2,raw = TRUE),data = bootstrapData)
  bs <- c(bs,coef(model_bootstrap)[2])
}
hist(bs)
bss <- sort(bs)
ci95 <- c(bss[round(bo*0.25)],bss[round(bo*0.975)])
ci95


#d bootstrap using boot package
beta1 <- function(data,i){
  model <- lm(Yield~poly(Fertilizer,2,raw = TRUE),subset=i,data = data)
  coef(model)[2]
}

cb <- boot(data,beta1,R=10000)
perc <- boot.ci(cb,type = "perc")
bca <- boot.ci(cb,type = "bca")
perc
#result kable
result <- c(CI[2,],95.3744,ci95,87.0976,perc$percent[4],perc$percent[5],87.3948,bca$bca[4],bca$bca[5],87.7167)
result_mt <- matrix(result,byrow = TRUE,ncol=3)
rownames(result_mt) <- c("lm","bootstrap","boot_percentile","boot_BCa")
colnames(result_mt) <- c("Lower Bound","Upper Bound","Interval Width") 
kable(result_mt,caption = "95% Confidence Interval of different methods")
