

# now we have a matrix of standardised medians.
set.seed(20)

source('readData.R')
d <- readData()

mycluster <- kmeans(t(d[,]), 3, nstart=1000)
mycluster
mycluster$cluster
dim(t(d))
e <- t(d)
dim(e)
#e <- e[-6,]

cols <- c(rep('red',11),rep('black',9))

e.pca <- prcomp(e , center = T,scale. = T)
summary(e.pca)
screeplot(e.pca,type='lines') 
plot(e.pca$x[,1:2],col=cols)




y <- c(rep(1,11),rep(0,9))
m <- glm(factor(y)~ e.pca$x[,1] +  e.pca$x[,2 ]   ,family=binomial)
AIC(m)
summary(m)
anova(m,test='Chisq')
 dim(e.pca$x[,1:2])
p <- predict(m,type='response')
plot(p,col=cols,ylab='predicted probability of being a patient',
     xlab='participant',main='within sample')
abline(h=0.5)
abline(v=11.5)
library(pROC)
e.roc <- roc(y,p)
plot(e.roc)
auc(e.roc)

ee <- cbind(e.pca$x[1,],e.pca$x[,2])
dim(ee)

mycluster <- kmeans(ee, 2, nstart=1000)
mycluster
mycluster$cluster
cols2 <- rep('red',20)
cols2[which(mycluster$cluster==2)] <- 'black'
plot(e.pca$x[,1:2],col=cols2)
plot(e.pca$x[,1:2],col=cols)

