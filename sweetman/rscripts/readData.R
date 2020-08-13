rm(list=ls())
setwd('~/Documents/sweetman/rscripts')

readData <- function(scale=T) {

f <- read.csv("../data/19021_FullList_NormalisedIntensities.csv",header=T)
dim(f)
colnames(f)
f$P1.mean <- NA
f$P2.mean <- NA
f$P3.mean <- NA
f$P4.mean <- NA
f$P5.mean <- NA
f$P6.mean <- NA
f$P7.mean <- NA
f$P8.mean <- NA
f$P9.mean <- NA
f$P10.mean <- NA
f$P11.mean <- NA

f$C1.mean <- NA
f$C2.mean <- NA
f$C3.mean <- NA
f$C4.mean <- NA
f$C5.mean <- NA
f$C6.mean <- NA
f$C7.mean <- NA
f$C8.mean <- NA
f$C9.mean <- NA


for (i in 1:dim(f)[1]) {
  f$P1.mean[i] <- mean(as.numeric(c(f$P1[i],f$P1.1[i],f$P1.2[i]))) 
  f$P2.mean[i] <- mean(as.numeric(c(f$P2[i],f$P2.1[i],f$P2.2[i]))) 
  f$P3.mean[i] <- mean(as.numeric(c(f$P3[i],f$P3.1[i],f$P3.2[i]))) 
  f$P4.mean[i] <- mean(as.numeric(c(f$P4[i],f$P4.1[i],f$P4.2[i]))) 
  f$P5.mean[i] <- mean(as.numeric(c(f$P5[i],f$P5.1[i],f$P5.2[i]))) 
  f$P6.mean[i] <- mean(as.numeric(c(f$P6[i],f$P6.1[i],f$P6.2[i]))) 
  f$P7.mean[i] <- mean(as.numeric(c(f$P7[i],f$P7.1[i],f$P7.2[i]))) 
  f$P8.mean[i] <- mean(as.numeric(c(f$P8[i],f$P8.1[i],f$P8.2[i]))) 
  f$P9.mean[i] <- mean(as.numeric(c(f$P9[i],f$P9.1[i],f$P9.2[i]))) 
  f$P10.mean[i] <- mean(as.numeric(c(f$P10[i],f$P10.1[i],f$P10.2[i]))) 
  f$P11.mean[i] <- mean(as.numeric(c(f$P11[i],f$P11.1[i],f$P11.2[i]))) 
  f$C1.mean[i] <- mean(as.numeric(c(f$C1[i],f$C1.1[i],f$C1.2[i]))) 
  f$C2.mean[i] <- mean(as.numeric(c(f$C2[i],f$C2.1[i],f$C2.2[i]))) 
  f$C3.mean[i] <- mean(as.numeric(c(f$C3[i],f$C3.1[i],f$C3.2[i]))) 
  f$C4.mean[i] <- mean(as.numeric(c(f$C4[i],f$C4.1[i],f$C4.2[i]))) 
  f$C5.mean[i] <- mean(as.numeric(c(f$C5[i],f$C5.1[i],f$C5.2[i]))) 
  f$C6.mean[i] <- mean(as.numeric(c(f$C6[i],f$C6.1[i],f$C6.2[i]))) 
  f$C7.mean[i] <- mean(as.numeric(c(f$C7[i],f$C7.1[i],f$C7.2[i]))) 
  f$C8.mean[i] <- mean(as.numeric(c(f$C8[i],f$C8.1[i],f$C8.2[i]))) 
  f$C9.mean[i] <- mean(as.numeric(c(f$C9[i],f$C9.1[i],f$C9.2[i]))) 
}

d <- matrix(#f$Index,f$Peak.Name,f$Group,
  c(f$P1.mean,f$P2.mean,f$P3.mean,f$P4.mean,
    f$P5.mean,f$P6.mean,f$P7.mean,f$P8.mean,
    f$P9.mean,f$P10.mean,f$P11.mean,
    f$C1.mean,f$C2.mean,f$C3.mean,f$C4.mean,
    f$C5.mean,f$C6.mean,f$C7.mean,f$C8.mean,f$C9.mean),
  nrow=dim(f)[1],ncol=20,byrow = F)
dim(d)
colnames(d) <- c('p1','p2','p3','p4','p5','p6',
                 'p7','p8','p9','p10','p11',
                 'c1','c2','c3','c4','c5','c6','c7','c8','c9')

##
## scale 
##

if (scale==T) {
  for (i in 1:dim(d)[1]) {
    mn <- mean(d[i,])
    sd <- sqrt(var(d[i,]))
    for (j in 1:dim(d)[2]) {
      # pareto clustering: divide by sqrt(sd)
      d[i,j] <- (d[i,j] - mn) / sqrt(sd)
    }
  }
}

return(d)
}


getGroupsByRows <- function(ids) {
  rows = as.numeric(as.character(ids$rowname))
  f <- read.csv("../data/19021_FullList_NormalisedIntensities.csv",header=T)
  return(f$Group[rows])
}
