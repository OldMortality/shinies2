rm(list=ls())
setwd('~/Documents/sweetman/rscripts')
source('readData.R')

d <- readData(scale=F)
dim(d)
# now we have a matrix of standardised medians.

set.seed(20)


mycluster <- kmeans(t(d[,]), 4, nstart=1000)
mycluster
mycluster$cluster
cl <- mycluster$cluster
cl[which(cl==1)]

e <- t(d)
e <- e[,]
dim(e)
mean(e[,200])
var(e[,200])


e.pca <- prcomp(e, center = T, scale. = T)
range(e[6,])
range(e.pca$x[1,])

summary(e.pca)
plot(e.pca)
par(mfrow=c(2,1))
screeplot(e.pca)
plot(e.pca$x[,1:2],col=c(rep('red',9),rep('black',7)))
#points(e.pca$x[10,1],e.pca$x[10,2],pch='X')
biplot(e.pca)
loadings <- e.pca$rotation
head(loadings)
barplot(loadings[,"PC1"])
barplot(loadings[,"PC2"])

plot3d(e.pca$x[,1:3],col = t.cols,size=10)

library(dplyr)
library(pipeR)
a <- e.pca$rotation
library(tibble)
# show 15 most important rows for PC1
a %>% as.data.frame %>% rownames_to_column %>% 
  select(rowname, PC1, PC2) %>% arrange(desc(PC1)) %>%  head(15)


a %>% as.data.frame %>% rownames_to_column %>% 
  select(rowname, PC1, PC2) %>% arrange(desc(PC1)) %>>%  (~ z)


head(z,15)  
head(getGroupsByRows(z),15)


# show 15 most important rows for PC2
a %>% as.data.frame %>% rownames_to_column %>% 
  select(rowname, PC1, PC2) %>% arrange(desc(PC2)) %>% head(15)



#source("https://bioconductor.org/biocLite.R")
#biocLite("pcaMethods")
#library(pcaMethods)
# 
# if (!requireNamespace("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
BiocManager::install("pcaMethods")


#library(caret)

# 
# set.seed(998)
# inTraining <- createDataPartition(pres4$present, p = .75, list = FALSE)
# training <- pres4[ inTraining,]
# testing  <- pres4[-inTraining,]
# 
# 
# fitControl <- trainControl(## 10-fold CV
#   method = "repeatedcv",
#   number = 10,
#   ## repeated ten times
#   repeats = 10)
# 
# 
# 
# 
# gbmFit1 <- train(present ~ . , 
#                  offset=log(Abundance),
#                  data = training, 
#                  #                 method = "cforest", 
#                  #                 method='dwdPoly',
#                  method='vglmAdjCat',
#                  trControl = fitControl)
# ## This last option is actually one
# ## for gbm() that passes through
# #verbose = FALSE)
# gbmFit1
# 
# 
# predict(gbmFit1)
# 
d.clust <- t(d[,])
hc <- hclust(dist(d.clust),"ave")
plot(hc)


#BiocManager::ins4tall("M3C")
#library(M3C)
#tsne(pollen$data,colvec=c('gold'))

install.packages('Rtsne')
library(Rtsne)

## calling the installed package
#train<- read.csv(file.choose()) ## Choose the train.csv file downloaded from the link above  
library(Rtsne)
## Curating the database for analysis with both t-SNE and PCA
labels <- colnames(d)
#train$label<-as.factor(train$label)
## for plotting
#colors = rainbow(length(unique(train$label)))
#names(colors) = unique(labels)
e <- t(d)
dim(e)
## Executing the algorithm on curated data
set.seed(20)
#ix <- sample(1:2970,100,replace=F)


tsne <- Rtsne(e[,], dims = 2, perplexity=5, verbose=TRUE, max_iter = 500)
exeTimeTsne <- system.time(Rtsne(e[,ix], dims = 2, perplexity=5, verbose=TRUE, max_iter = 500))

## Plotting
plot(tsne$Y, t='n', main="tsne")
text(tsne$Y, labels=labels)

t.cols <- c(rep('red',11),rep('black',9))
t.cols[11] <- 'blue'
t.cols[16] <- 'blue'

tsne <- Rtsne(e[,], dims = 3, perplexity=5, verbose=TRUE, max_iter = 500)
exeTimeTsne <- system.time(Rtsne(e[,], dims = 2, perplexity=5, verbose=TRUE, max_iter = 500))

## Plotting
t.cols
library(rgl)
plot3d(tsne$Y,col = t.cols,size=10)
legend3d("topright", legend = '0':'5', pch = 16, col = rainbow(5))


heatmap(e, Rowv = NA, Colv = NA)  
my_colors <- colorRampPalette(c("cyan", "deeppink3")) 
heatmap(e, col = my_colors(100))

dim(e)
mydata.cor = cor(e)
dim(mydata.cor)
heatmap(mydata.cor, col = my_colors(100))


# cluster on pca 
mycluster <- kmeans(e.pca$x, 3, nstart=5000)
mycluster
mycluster$cluster
cl <- mycluster$cluster
cl[which(cl==1)]


###
### lda
###
library(MASS)
labels
e <- t(d)
dim(e)

labels = c(rep(1,11),rep(0,9))
#my.lda <- lda(labels ~ . , data=data.frame(e) )
#predict(my.lda)$class
library(e1071)
my.svm <- svm(labels ~ . , data=data.frame(e) )
plot(predict(my.svm))


