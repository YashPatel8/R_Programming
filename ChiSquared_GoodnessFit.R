
R11 <- runif(1000)
print(R11)
X11 <- -log(R11)
print(X11)
H11 <- hist(X11, plot=F)
library(fitdistrplus)
fit1 <- fitdist(X11, "exp")
plot(fit1)
dexp <- dexp(X11)
chisq.test(X11, dexp)


R21 <- runif(10000)
print(R21)
R22 <- runif(10000)
print(R22)
R23 <- runif(10000)
print(R23)
X21 <- -log(R21*R22*R23)
print(X21)
H21 <- hist(X21, plot=F)
fit2 <- fitdist(X21, "gamma")
plot(fit2)
dg <- dgamma(X21, shape = 1)
chisq.test(X21, dg)


R31 <- runif(1000)
print(R31)
R32 <- runif(1000)
print(R32)
X31 <- -log(R31)
print(X31)
X32 <- -log(R32)
print(X32)
K31 <- (((X31-1)^2)/2)
print(K31)
R <- vector()
Y31 <- vector()
for(i in 1:1000)
{
  if(K31[i] <= X32[i])
  {
    R[i] <- runif(1,0,1)
    if(R[i] > 0.5)
    {
      Y31[i] <- X31[i]
    }
    if(R[i] <= 0.5) 
    {
      Y31[i] <- -X31[i]
    }
  }
}
print(R)
print(Y31)
newR <- na.omit(R)
print(newR)
newY31 <- na.omit(Y31)
print(newY31)
newY31 <- as.vector(newY31)
fit3 <- fitdist(newY31, "norm")
plot(fit3)
normY31 <- dnorm(newY31)
chisq.test(newY31, normY31)

