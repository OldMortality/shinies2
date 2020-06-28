rm(list=ls())
source('readData.R')
d <- readData(scale=T)




leaveOneOut <- function(df,ix,method='pca') {

  leftOut <- df[,ix]
  leftOut2 <- matrix(leftOut,nrow=1,ncol=2970,byrow=T)
#  dim(leftOut2)
  df <- df[,-ix]
#  dim(df)
  
  e <- t(df)
#  dim(e)
  # cols <- c(rep('red',11),rep('black',9))
  # cols <- cols[-ix]
  y <- c(rep(1,11),rep(0,9))
 
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

se <- vector()
predictions <- vector()

method='pca'
for (ix in 1:dim(d)[2]) {
  leaveOne <- leaveOneOut(d,ix,method=method)
  se[ix] <- leaveOne$squared.error
  predictions[ix]<- leaveOne$prediction
}
plot(se,main=paste('Squared error ',method))
plot(predictions,main=paste('Prediction ',method))
abline(h=0.5)
abline(v=11.5)
abline(h=0.5,col='red')
grid()
MSE = sqrt(sum(se)/dim(d)[2])
MSE
library(pROC)
my.roc <- roc(labels,predictions)
plot(my.roc)
auc(my.roc)
