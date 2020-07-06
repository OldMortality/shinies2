#rm(list=ls())
setwd('~/Documents/sweetman/data')
mito.df <- read.csv("Mitochondrion proteins2.csv",header=T)
colnames(mito.df)
dim(mito.df)
ix <- mito.df$Index
#ix <- mito.df[which(mito.df$type=='ATP Synthesis proteins (n=10)'),"Index"]
length(ix)
ix
d.m <- d[ix,]
dim(d.m)

e.m <- t(d.m)
e.pca <- prcomp(e.m , center = T,scale. = T)
firstPC = e.pca$x[,1]
secondPC = e.pca$x[,2] 
thirdPC = e.pca$x[,3] 
y <- c(rep(1,11),rep(0,9))

#m <- glm(factor(y)~ e.pca$x[,1] +  e.pca$x[,2 ]   ,family=binomial)
m <- glm(factor(y[])~ firstPC +  secondPC + thirdPC    ,family=binomial,  maxit=100)
summary(m)
anova(m,test='Chisq')
# get principal components for leftOut
predict.pca <- predict(e.pca,newdata = leftOut2 )
p <- predict(m ,type='response')
plot(p)


mycluster <- kmeans(t(d.m[,]), 3, nstart=1000)
mycluster
mycluster$cluster
c <- mycluster$cluster
c[which(c==1)]
c[which(c==2)]
c[which(c==3)]

my_colors <- colorRampPalette(c("cyan", "deeppink3")) 
heatmap(e.m, col = my_colors(100))
heatmap(t(e.m), col = my_colors(100))

heatmap(d, col = my_colors(100))
heatmap(t(e.m), col = my_colors(100))


mydata.cor = cor(e.m)
heatmap(mydata.cor, col = my_colors(100))




# get the legend
m <- matrix(c(1,0,-1,-1,1,0,-1,0,1),nrow=3,ncol=3)
m
heatmap(m, col = my_colors(100),Rowv=F,Colv=F)
  

mydata.cor = cor(t(d))
heatmap(mydata.cor, col = my_colors(100))

patients <- d[,1:11]
controls <- d[,12:20]

par(mfrow=c(2,1))
mydata.cor = cor(t(patients))
heatmap(mydata.cor, col = my_colors(100),main='patients')
mydata.cor = cor(t(controls))
heatmap(mydata.cor, col = my_colors(100),main='controls')
