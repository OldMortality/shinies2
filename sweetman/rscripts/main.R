rm(list=ls())
setwd('~/Documents/sweetman/rscripts')
source('readData.R')
d <- readData(scale=T)
e <- t(d)
dim(e)
cols <- c(rep('red',11),rep('black',9))
e.pca <- prcomp(e , center = F,scale. = F)
summary(e.pca)
screeplot(e.pca,type='lines') 
plot(e.pca$x[,1:2],col=cols)

## cross validation

leaveOneOut <- function(df,ix,y) {
  
  leftOut <- df[,ix]
  leftOut2 <- matrix(leftOut,nrow=1,ncol=2970,byrow=T)
  df <- df[,-ix]
  e <- t(df)
  # truth of the one we are going to predict
  yLeftOut = y[ix]
  y <- y[-ix]
  e.pca <- prcomp(e , center = T,scale. = T)
  firstPC = e.pca$x[,1]
  secondPC = e.pca$x[,2] 
  thirdPC = e.pca$x[,3] 
  m <- glm(factor(y[])~ firstPC +  secondPC + thirdPC    ,family=binomial,  maxit=100)
  #summary(m)
  #anova(m,test='Chisq')
  # get principal components for leftOut
  predict.pca <- predict(e.pca,newdata = leftOut2 )
  # predict probability for the one left out
  p <- predict(m,newdata=data.frame(firstPC = predict.pca[1],
                                    secondPC  = predict.pca[2],
                                    thirdPC  = predict.pca[3]
  ),type='response')
  # Brier score
  SE = (p - yLeftOut)^2
  return(list(squared.error=SE,
              prediction=p))
  
} 

# get the number of false positives and false negatives
getErrs <- function(predictions, truth) {
  fn <- sum(predictions < 0.5 & truth == 1)
  fp <- sum(predictions > 0.5 & truth == 0)
  return(fn+fp)
}



predictions <- vector()
trueLabels <-c(rep(1,11),rep(0,9))
for (ix in 1:dim(d)[2]) {
  leaveOne <- leaveOneOut(d,ix,y=y <- trueLabels )
  predictions[ix]<- leaveOne$prediction
}
# a better plot is in plotit.R
plot(predictions,main=paste('Prediction ',method))
abline(h=0.5)
abline(v=11.5)
abline(h=0.5,col='red')
grid()
predictions.saved <- predictions

# number of prediction errors, with threshold = 0.5
getErrs(predictions,trueLabels)


## randomly permute labels to see how good 4 errors is
errs.r <- vector()
N.SIM <- 1000
for (r in 1:N.SIM) {
  print(r)
  ix <- sample(1:dim(d)[2])
  permutedLabels <- rep(0,20)
  permutedLabels[ix[1:11]] <- 1
  # xval
  for (ix in 1:dim(d)[2]) {
    leaveOne <- leaveOneOut(d,ix,y=y <- permutedLabels )
    predictions[ix]<- leaveOne$prediction
  }
  errs.r[r] <- getErrs(predictions,permutedLabels)
}

hist(errs.r,30)
abline(v=4,col='red')
# get p-value: add those as extreme or more than 3 errors
t1 <- sum(errs.r <= 3 )
t2 <- sum(errs.r >= 17)
t1 + t2
(t1 + t2)/N.SIM


mean(errs.r)


