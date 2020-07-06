rm(list=ls())
setwd('~/Documents/sweetman/rscripts')
source('readData.R')
d <- readData(scale=T)




leaveOneOut <- function(df,ix,method='pca',y) {

  leftOut <- df[,ix]
  leftOut2 <- matrix(leftOut,nrow=1,ncol=2970,byrow=T)
#  dim(leftOut2)
  df <- df[,-ix]
#  dim(df)
  
  e <- t(df)
#  dim(e)
  # cols <- c(rep('red',11),rep('black',9))
  # cols <- cols[-ix]
  
   # truth of the one we are going to predict
  yLeftOut = y[ix]
  y <- y[-ix]
  
  if (method=='pca') {
    e.pca <- prcomp(e , center = T,scale. = T)
    firstPC = e.pca$x[,1]
    secondPC = e.pca$x[,2] 
    thirdPC = e.pca$x[,3] 
    
    
    #m <- glm(factor(y)~ e.pca$x[,1] +  e.pca$x[,2 ]   ,family=binomial)
    m <- glm(factor(y[])~ firstPC +  secondPC + thirdPC    ,family=binomial,  maxit=100)
    summary(m)
    anova(m,test='Chisq')
    # get principal components for leftOut
    predict.pca <- predict(e.pca,newdata = leftOut2 )
    p <- predict(m,newdata=data.frame(firstPC = predict.pca[1],
                                      secondPC  = predict.pca[2],
                                      thirdPC  = predict.pca[3]
                                      
    ),type='response')
    
  } 
  if (method=='svm') {
    my.svm <- svm(y ~ . , data = data.frame(e) )
    p <- predict(my.svm,newdata=leftOut2)
    
  }
  
  # Brier score
  SE = (p - yLeftOut)^2
  return(list(squared.error=SE,
              prediction=p))
  
  
} 

getErrs <- function(predictions, truth) {
  t1 <- sum(predictions < 0.5 & truth == 1)
  t2 <- sum(predictions > 0.5 & truth == 0)
  return(t1 + t2)
  
}

getErrs(c(0.8,0.4,0.8),c(0,1,1))



se <- vector()
predictions <- vector()

trueLabels <-c(rep(1,11),rep(0,9))
method='pca'
for (ix in 1:dim(d)[2]) {
  leaveOne <- leaveOneOut(d,ix,method=method,y=y <- trueLabels )
  se[ix] <- leaveOne$squared.error
  predictions[ix]<- leaveOne$prediction
}
plot(se,main=paste('Squared error ',method))
plot(predictions,main=paste('Prediction ',method))
abline(h=0.5)
abline(v=11.5)
abline(h=0.5,col='red')
grid()

# number of prediction errors, with threshold = 0.5
getErrs(predictions,trueLabels)


## randomly permute labels to see how good 4 errors is
errs.r <- vector()
for (r in 1:1000) {
  print(r)
  ix <- sample(1:dim(d)[2])
  permutedLabels <- rep(0,20)
  permutedLabels[ix[1:11]] <- 1
  # xval
  for (ix in 1:dim(d)[2]) {
    leaveOne <- leaveOneOut(d,ix,method=method,y=y <- permutedLabels )
    predictions[ix]<- leaveOne$prediction
  }
  errs.r[r] <- getErrs(predictions,permutedLabels)
}

hist(errs.r,30)
abline(v=4,col='red')
t1 <- sum(errs.r < 4 )
t2 <- sum(errs.r >= 17)
t1 + t2
(t1 + t2)/1000
mean(errs.r)

library(ggplot2)
d.plot <- data.frame(x=seq(1,20),y=predictions,
                     cols=c(rep('case',11),rep('control',9)))
ggplot(data=d.plot,aes(x=x,y=y,colour=cols)) + geom_point() + 
  geom_hline(yintercept=0.5) + 
  geom_vline(xintercept=11.5) + 
  labs(x='participant',y="Predicted probability of being a case") #+
  #annotate("text", x = c(5,17), y=c(1.2,1.2), label = c("cases", "controls"))


MSE = sqrt(sum(se)/dim(d)[2])
MSE
library(pROC)
my.roc <- roc(labels,predictions)
plot(my.roc)
auc(my.roc)
