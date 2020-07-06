N <- 3000
M <- 120
x <- rnorm(N*M,sd=10)
m <- matrix(x,nrow=M,ncol=N)
dim(m)
for (i in 1:dim(m)[1]) {
  mn <- mean(m[i,])
  sd <- sqrt(var(m[i,]))
  for (j in 1:dim(m)[2]) {
    m[i,j] <- (m[i,j] - mn) / sd
  }
}

dim(m)
x.pca <- prcomp(m)
plot(x.pca)
plot(x.pca$x[,1],x.pca$x[,2])



m = matrix  (c(80,85,60,55, 
               90,85,70,45,95,80,40,50),nrow=3,ncol=4,byrow=T)
m
rownames(m) <- c("john","mike","kate")
m.pca  <- prcomp(m,scale=T)
m.pca$rotation
m.pca$x[1,]

