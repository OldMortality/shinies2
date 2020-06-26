rm(list=ls())
setwd('~/Documents/sweetman/rscripts')
f <- read.csv("../data/19021_FullList_NormalisedIntensities.csv",header=T)
dim(f)
colnames(f)
f$P1.median <- NA
f$P2.median <- NA
f$P3.median <- NA
f$P4.median <- NA
f$P5.median <- NA
f$P6.median <- NA
f$P7.median <- NA
f$P8.median <- NA
f$P9.median <- NA
f$P10.median <- NA
f$P11.median <- NA

f$C1.median <- NA
f$C2.median <- NA
f$C3.median <- NA
f$C4.median <- NA
f$C5.median <- NA
f$C6.median <- NA
f$C7.median <- NA
f$C8.median <- NA
f$C9.median <- NA


for (i in 1:dim(f)[1]) {
  f$P1.median[i] <- median(as.numeric(c(f$P1[i],f$P1.1[i],f$P1.2[i]))) 
  f$P2.median[i] <- median(as.numeric(c(f$P2[i],f$P2.1[i],f$P2.2[i]))) 
  f$P3.median[i] <- median(as.numeric(c(f$P3[i],f$P3.1[i],f$P3.2[i]))) 
  f$P4.median[i] <- median(as.numeric(c(f$P4[i],f$P4.1[i],f$P4.2[i]))) 
  f$P5.median[i] <- median(as.numeric(c(f$P5[i],f$P5.1[i],f$P5.2[i]))) 
  f$P6.median[i] <- median(as.numeric(c(f$P6[i],f$P6.1[i],f$P6.2[i]))) 
  f$P7.median[i] <- median(as.numeric(c(f$P7[i],f$P7.1[i],f$P7.2[i]))) 
  f$P8.median[i] <- median(as.numeric(c(f$P8[i],f$P8.1[i],f$P8.2[i]))) 
  f$P9.median[i] <- median(as.numeric(c(f$P9[i],f$P9.1[i],f$P9.2[i]))) 
  f$P10.median[i] <- median(as.numeric(c(f$P10[i],f$P10.1[i],f$P10.2[i]))) 
  f$P11.median[i] <- median(as.numeric(c(f$P11[i],f$P11.1[i],f$P11.2[i]))) 
  f$C1.median[i] <- median(as.numeric(c(f$C1[i],f$C1.1[i],f$C1.2[i]))) 
  f$C2.median[i] <- median(as.numeric(c(f$C2[i],f$C2.1[i],f$C2.2[i]))) 
  f$C3.median[i] <- median(as.numeric(c(f$C3[i],f$C3.1[i],f$C3.2[i]))) 
  f$C4.median[i] <- median(as.numeric(c(f$C4[i],f$C4.1[i],f$C4.2[i]))) 
  f$C5.median[i] <- median(as.numeric(c(f$C5[i],f$C5.1[i],f$C5.2[i]))) 
  f$C6.median[i] <- median(as.numeric(c(f$C6[i],f$C6.1[i],f$C6.2[i]))) 
  f$C7.median[i] <- median(as.numeric(c(f$C7[i],f$C7.1[i],f$C7.2[i]))) 
  f$C8.median[i] <- median(as.numeric(c(f$C8[i],f$C8.1[i],f$C8.2[i]))) 
  f$C9.median[i] <- median(as.numeric(c(f$C9[i],f$C9.1[i],f$C9.2[i]))) 
}

d <- matrix(#f$Index,f$Peak.Name,f$Group,
  c(f$P1.median,f$P2.median,f$P3.median,f$P4.median,
    f$P5.median,f$P6.median,f$P7.median,f$P8.median,
    f$P9.median,f$P10.median,f$P11.median,
    f$C1.median,f$C2.median,f$C3.median,f$C4.median,
    f$C5.median,f$C6.median,f$C7.median,f$C8.median,f$C9.median),
  nrow=dim(f)[1],ncol=20,byrow = F)
dim(d)
colnames(d) <- c('p1','p2','p3','p4','p5','p6',
                 'p7','p8','p9','p10','p11',
                 'c1','c2','c3','c4','c5','c6','c7','c8','c9')
df = d
# scale
for (i in 1:dim(df)[1]) {
  mn <- mean(df[i,])
  sd <- sqrt(var(df[i,]))
  for (j in 1:dim(df)[2]) {
    df[i,j] <- (df[i,j] - mn) / sd
  }
}





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
plot(predictions,main=paste('Squared error ',method))
abline(h=0.5,col='red')
grid()
MSE = sqrt(sum(se)/dim(d)[2])
MSE
library(pROC)
my.roc <- roc(labels,predictions)
plot(my.roc)
auc(my.roc)
