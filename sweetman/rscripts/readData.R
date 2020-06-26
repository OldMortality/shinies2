rm(list=ls())
setwd('~/Documents/sweetman/rscripts')

readData <- function() {

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

##
## scale 
##
for (i in 1:dim(d)[1]) {
  mn <- mean(d[i,])
  sd <- sqrt(var(d[i,]))
  for (j in 1:dim(d)[2]) {
    d[i,j] <- (d[i,j] - mn) / sd
  }
}

return(d)
}
