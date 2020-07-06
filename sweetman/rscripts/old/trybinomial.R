invlogit <- function(x) {
  1/(1+exp(-x))
}
#invlogit(0)

x <- rnorm(100)
y <- rnorm(100)
eta = 2 * x + 3 * y
p <- invlogit(eta)
z <- rbinom(100,1,p)
glm(z~eta, family=binomial)
head(cbind(x,y,z))
df.l <- data.frame(x=x,y=y,eta=eta,z=z)
head(df.l)
df.l[1,] <- df.l[2,]
df.l[1,'z'] <- 0
head(df.l)
glm(z~eta, family=binomial,data=df.l)
