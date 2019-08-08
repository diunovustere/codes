install.packages("rmutil")
install.packages("lpSolve")
install.packages("quantmod")
install.packages("MASS")

library(rmutil)
library(lpSolve)
library(quantmod)
library(MASS)

getSymbols("AMD")
data.frame("AMD")
open <- as.numeric(AMD$AMD.Open)
close <- as.numeric(AMD$AMD.Close)
high <- as.numeric(AMD$AMD.High)
low <- as.numeric(AMD$AMD.Low)
barChart(AMD,theme='white.mono',bar.type='hlc') 


openReturn <- (open[-1]-open[-length(open)])/open[-length(open)]
closeReturn <- (close[-1]-close[-length(close)])/close[-length(close)]
highReturn <- (high[-1]-high[-length(high)])/high[-length(high)]
lowReturn <- (low[-1]-low[-length(low)])/low[-length(low)]


#### openReturn
testmeanopen <- mean(openReturn) ##0.00072
testSDopen <- sd(openReturn) ##0.03812;
dnorm(openReturn,testmeanopen,testSDopen)
plot(dnorm)
hist(openReturn, freq= FALSE)
hist(openReturn, prob=TRUE, col="red")
lines(density(openReturn), col="blue", lwd=2) 
par(new=TRUE)
testNormopen <- rnorm(100000, testmeanopen, testSDopen)
ks.test(openReturn, testNormopen)
testChiopen <- rchisq (100000, testmeanopen)
ks.test(openReturn, testChiopen)

###Two-sample Kolmogorov-Smirnov test

##data:  openReturn and testChiopen
##D = 0.48999, p-value < 2.2e-16
##alternative hypothesis: two-sided
##############end openReturn


### closeReturn
testmeanclose <- mean(closeReturn) ##0.00730847
testSDclose <- sd(closeReturn)   ##0.038267
dnorm(closeReturn,testmeanclose,testSDclose)
plot(dnorm)
hist(closeReturn, freq= FALSE)
hist(closeReturn, prob=TRUE, col="red")
lines(density(closeReturn), col="blue", lwd=2) 
par(new=TRUE)
testNormclose <- rnorm(100000, testmeanclose, testSDclose)
ks.test(closeReturn, testNormclose)
testChiclose <- rchisq (100000, testmeanclose)
ks.test(closeReturn, testChiclose)
hist(closeReturn)
hist(closeReturn, prob=TRUE, col="red")
lines(density(closeReturn), col="blue", lwd=2) 

##Two-sample Kolmogorov-Smirnov test

###data:  closeReturn and testChiclose
###D = 0.48498, p-value < 2.2e-16
###alternative hypothesis: two-sided


###end closeReturn
###    highReturn
testmeanhigh <- mean(highReturn) ##0.0005862
testSDhigh <- sd(highReturn)   ##0.034769
dnorm(highReturn,testmeanhigh,testSDhigh)
plot(dnorm)
hist(highReturn, freq= FALSE)
hist(highReturn, prob=TRUE, col="red")
lines(density(highReturn), col="blue", lwd=2) 
par(new=TRUE)
testNormhigh <- rnorm(100000, testmeanhigh, testSDhigh)
ks.test(highReturn, testNormhigh)
testChihigh <- rchisq (100000, testmeanhigh)
ks.test(highReturn, testChihigh)
hist(highReturn)
hist(highReturn, prob=TRUE, col="red")
lines(density(highReturn), col="blue", lwd=2) 
####Two-sample Kolmogorov-Smirnov test

#### data:  highReturn and testChihigh
##### D = 0.51168, p-value < 2.2e-16
##### alternative hypothesis: two-sided

###end highreturn


###low return

testmeanlow <- mean(lowReturn) ##0.00064325
testSDlow <- sd(lowReturn)   ##0.0357054
dnorm(lowReturn,testmeanlow,testSDlow)
plot(dnorm)
hist(lowReturn, freq= FALSE)
hist(lowReturn, prob=TRUE, col="red")
lines(density(lowReturn), col="blue", lwd=2) 
par(new=TRUE)
testNormlow <- rnorm(100000, testmeanlow, testSDlow)
ks.test(lowReturn, testNormlow)
testChilow <- rchisq (100000, testmeanlow)
ks.test(lowReturn, testChilow)
hist(lowReturn)
hist(lowReturn, prob=TRUE, col="red")
lines(density(lowReturn), col="blue", lwd=2) 

##Two-sample Kolmogorov-Smirnov test

##data:  lowReturn and testNormlow
##D = 0.07972, p-value = 2.22e-16
##alternative hypothesis: two-sided



##conclusion: best way to calculate stocks would be based off of chi square analysis since we are grabbing random samples and comparing to real live data.
## normal distribution (while it may) but usually, does not occur when eveluating stocks since there are multiple contributions to the rising and falling of the stock.
