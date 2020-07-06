d <- readData(scale=F)
ps <- vector()
ts <- vector()
# i = 974
# i = 492
# i = 1916
for (i in 1:dim(d)[1]) {
  t <- t.test(d[i,1:11],d[i,12:20])

  ps[i] <- t$p.value
  ts[i] <- t$statistic
}
hist(ts,30,prob=T)
hist(ps,30,prob=T)

df.p <- data.frame(p=ps,ix = 1:dim(d)[1])
head(df.p)
tail(df.p)

order.p <- order(ps)
df.p <- df.p[order.p,]
head(df.p)
tail(df.p)

plot(df.p$p,pch='.')
plot(df.p$p,pch='.',xlim=c(0,100),ylim=c(0,0.05))
q <- 0.1
abline(0,q /dim(d)[1],col='red')

plot(df.p$p,pch='.',xlim=c(0,100),ylim=c(0,0.05))
abline(0,q /dim(d)[1],col='red')
# BH: i.max <- largest i : p <- i * q / N
#  reject H0 for i < i.max
head(df.p)
df.p$BH <- NA
df.p$rejectBH <- F

for (i in 1:dim(df.p)[1]) {
  df.p$BH[i] <- q * i / dim(df.p)[1]
  if (df.p$p[i] < df.p$BH[i]) {
    df.p$rejectBH[i] <- T
  }
}
head(df.p$rejectBH,100)
which(df.p$rejectBH==T)
