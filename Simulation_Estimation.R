#problem 1
install.packages("triangle")
library(triangle)
triangle <- rtriangle(5000,20,300,80)
print(triangle)
round <- round(triangle)
print(round)
hist(triangle)
summary(triangle)

#part 1.1
bethisr <- vector()
tufts <- vector()
massgen <- vector()
bosmed <- vector()
brigham <- vector()
for(i in 1:5000)
{
  bethisr[i] <- 30/100*round[i]
  tufts[i] <- 15/100*round[i]
  massgen[i] <- 20/100*round[i]
  bosmed[i] <- 25/100*round[i]
  brigham[i] <- 10/100*round[i]
}
print(bethisr)
mean(bethisr)
print(tufts)
mean(tufts)
print(massgen)
mean(massgen)
print(bosmed)
mean(bosmed)
print(brigham)
mean(brigham)

#part 1.2
rate_bethisr <- rexp(5000, rate = 1/7)
rate_tufts <- rexp(5000, rate = 1/10)
rate_massgen <- rexp(5000, rate = 1/15)
rate_bosmed <- rexp(5000, rate = 1/15)
rate_brigham <- rexp(5000, rate = 1/20)
time_bethisr <- vector()
time_tufts <- vector()
time_massgen <- vector()
time_bosmed <- vector()
time_brigham <- vector()
for(i in 1: 5000)
{
  time_bethisr[i] <- rate_bethisr[i] * bethisr[i]
  time_tufts[i] <- rate_tufts[i] * tufts[i]
  time_massgen[i] <- rate_massgen[i] * massgen[i]
  time_bosmed[i] <- rate_bosmed[i] * bosmed[i]
  time_brigham[i] <- rate_brigham[i] * brigham[i]
}
timehr_bethisr <- time_bethisr/60
print(time_bethisr)
mean(time_bethisr)
timehr_tufts <- time_tufts/60
print(time_tufts)
mean(time_tufts)
timehr_massgen <- time_massgen/60
print(time_massgen)
mean(time_massgen)
timehr_bosmed <- time_bosmed/60
print(time_bosmed)
mean(time_bosmed)
timehr_brigham <- time_brigham/60
print(time_brigham)
mean(time_brigham)

#part 1.3
ob_avg <- rep(0,5000)
no_rep <- rep(0,5000)
for (i in 1:5000)
{
  th_avg <- mean(30/100 * rtriangle(i,20,300,80))
  ob_avg[i] <- th_avg
  no_rep[i] <- i
}
print(th_avg)
plot(no_rep,ob_avg)

#part 1.4
#sub-part 1.4.1
mean_bethisr <- mean(timehr_bethisr)
print(mean_bethisr)
ci_5 <- qgamma(0.05,5000,5000)
print(ci_5)
ci_95 <- qgamma(0.95,5000,5000)
print(ci_95)
conf_int <- c(ci_5/mean_bethisr,ci_95/mean_bethisr)
print(conf_int)

#sub-part 1.4.2
#the data is continuous and asymmetric. And mostly have positive outliers. Hence gamma distribution.

#sub-part 1.4.3
install.packages("fitdistrplus")
library(fitdistrplus)
fit_bethisr <- fitdist(timehr_bethisr,"gamma")
plot(fit_bethisr)
gamma_bethisr <- dgamma(timehr_bethisr, shape = 1)
chisq.test(timehr_bethisr, gamma_bethisr)

#part 1.5
total_victims <- bethisr + tufts + massgen + bosmed + brigham
print(total_victims)
total_time <- round(time_bethisr) + round(time_tufts) + round(time_massgen) + round(time_bosmed) + round(time_brigham)
print(total_time)
t <- total_time/total_victims
print(t)
summary(t)

tci_5 <- qgamma(0.05,5000,5000)
print(tci_5)
tci_95 <- qgamma(0.95,5000,5000)
print(tci_95)
tconf_int <- c(tci_5/t,tci_95/t)
print(tconf_int)

fit_t <- fitdist(t,"gamma")
plot(fit_t)
gamma_t <- dgamma(t, shape = 1)
chisq.test(t, gamma_t)


#problem 2
install.packages("triangle")
library(triangle)
normal <- rnorm(5000,150.50)
print(normal)
round <- round(normal)
print(round)
hist(normal)
summary(normal)

#part 2.1
nbethisr <- vector()
ntufts <- vector()
nmassgen <- vector()
nbosmed <- vector()
nbrigham <- vector()
for(i in 1:5000)
{
  nbethisr[i] <- 30/100*round[i]
  ntufts[i] <- 15/100*round[i]
  nmassgen[i] <- 20/100*round[i]
  nbosmed[i] <- 25/100*round[i]
  nbrigham[i] <- 10/100*round[i]
}
print(nbethisr)
mean(nbethisr)
print(ntufts)
mean(ntufts)
print(nmassgen)
mean(nmassgen)
print(nbosmed)
mean(nbosmed)
print(nbrigham)
mean(nbrigham)

#part 2.2
rate_nbethisr <- rnorm(5000, 7, 2)
rate_ntufts <- rnorm(5000, 10, 4)
rate_nmassgen <- rnorm(5000, 15, 3)
rate_nbosmed <- rnorm(5000, 15, 5)
rate_nbrigham <- rnorm(5000, 20, 3)
time_nbethisr <- vector()
time_ntufts <- vector()
time_nmassgen <- vector()
time_nbosmed <- vector()
time_nbrigham <- vector()
for(i in 1: 5000)
{
  time_nbethisr[i] <- rate_nbethisr[i] * nbethisr[i]
  time_ntufts[i] <- rate_ntufts[i] * ntufts[i]
  time_nmassgen[i] <- rate_nmassgen[i] * nmassgen[i]
  time_nbosmed[i] <- rate_nbosmed[i] * nbosmed[i]
  time_nbrigham[i] <- rate_nbrigham[i] * nbrigham[i]
}
timehr_nbethisr <- time_nbethisr/60
print(time_nbethisr)
mean(time_nbethisr)
timehr_ntufts <- time_ntufts/60
print(time_ntufts)
mean(time_ntufts)
timehr_nmassgen <- time_nmassgen/60
print(time_nmassgen)
mean(time_nmassgen)
timehr_nbosmed <- time_nbosmed/60
print(time_nbosmed)
mean(time_nbosmed)
timehr_nbrigham <- time_nbrigham/60
print(time_nbrigham)
mean(time_nbrigham)

#part 2.3
ob_navg <- rep(0,5000)
no_nrep <- rep(0,5000)
for (i in 1:5000)
{
  th_navg <- mean(30/100 * rnorm(i, 150, 50))
  ob_navg[i] <- th_navg
  no_nrep[i] <- i
}
print(th_navg)
plot(no_nrep,ob_navg)

#part 2.4
#sub-part 2.4.1
mean_nbethisr <- mean(timehr_nbethisr)
print(mean_nbethisr)
nci_5 <- qgamma(0.05,5000,5000)
print(nci_5)
nci_95 <- qgamma(0.95,5000,5000)
print(nci_95)
nconf_int <- c(nci_5/mean_nbethisr,nci_95/mean_nbethisr)
print(nconf_int)

#sub-part 2.4.2
#the data is continuous and asymmetric. And mostly have positive outliers. Hence gamma distribution.

#sub-part 2.4.3
install.packages("fitdistrplus")
library(fitdistrplus)
fit_nbethisr <- fitdist(timehr_nbethisr,"gamma")
plot(fit_nbethisr)
gamma_nbethisr <- dgamma(timehr_nbethisr, shape = 1)
chisq.test(timehr_nbethisr, gamma_nbethisr)

#part 2.5
ntotal_victims <- nbethisr + ntufts + nmassgen + nbosmed + nbrigham
print(ntotal_victims)
ntotal_time <- round(time_nbethisr) + round(time_ntufts) + round(time_nmassgen) + round(time_nbosmed) + round(time_nbrigham)
print(ntotal_time)
summary(ntotal_time)
nt <- ntotal_time/ntotal_victims
print(nt)
summary(nt)

ntci_5 <- qgamma(0.05,5000,5000)
print(ntci_5)
ntci_95 <- qgamma(0.95,5000,5000)
print(ntci_95)
ntconf_int <- c(ntci_5/nt,ntci_95/nt)
print(ntconf_int)

fit_nt <- fitdist(nt,"gamma")
plot(fit_nt)
gamma_nt <- dgamma(nt, shape = 1)
chisq.test(t, gamma_nt)
