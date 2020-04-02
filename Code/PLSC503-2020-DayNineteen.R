########################################################
# PLSC 503 -- Spring 2020: Code for Day Nineteen
########################################################

# Libraries (install as necessary)

library(RCurl)
library(maxLik)
library(aod)
library(lmtest)
library(car)

# Get COVID data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/COVID-PA.csv")
COVID<-read.csv(text=temp, header=TRUE)
rm(temp)

# log-lik function:

COVIDll <- function(param) {
  mu <- param[1]
  sigma <- param[2]
  ll <- -0.5*log(sigma^2) - (0.5*((x-mu)^2/sigma^2))
  ll
}

x<-log(COVID$CasesPer10K+0.055)

# What does that look like?

pdf("COVIDdensity.pdf",7,6)
par(mar=c(4,4,2,2))
plot(density(x),main="",lwd=2,
     xlab="log(COVID cases per 10K population)")
abline(v=0,lty=2)
dev.off()

# fit the model:

hats <- maxLik(COVIDll, start=c(0,1))
summary(hats)

# mean-only linear model for comparison:

COVIDLM<-lm(x~1)
summary(COVIDLM)

# components:

hats$estimate
hats$gradient
hats$hessian

-(solve(hats$hessian))
sqrt(-(solve(hats$hessian)))

# Wald test:

wald.test(Sigma=vcov(hats),b=coef(hats),Terms=1:2,verbose=TRUE)

# More Wald tests:

wald.test(Sigma=vcov(hats),b=coef(hats),Terms=1:2,H0=c(0,2))

wald.test(Sigma=vcov(hats),b=coef(hats),Terms=1:2,H0=c(-0.2,1.5))

# nonsensical:

wald.test(Sigma=vcov(hats),b=coef(hats),Terms=1:2,H0=c(-0.2,-0.1))

# LR test...

COVIDllAlt <- function(param) {
    sigma <- param[1]
  ll <- -0.5*log(sigma^2) - (0.5*((x-0)^2/sigma^2))
  ll
}

hatsF <- maxLik(COVIDll, start=c(0,1))
hatsR <- maxLik(COVIDllAlt, start=c(1))

hatsF$maximum
hatsR$maximum

-2*(hatsR$maximum-hatsF$maximum)

pchisq(-2*(hatsR$maximum-hatsF$maximum),df=1,lower.tail=FALSE)

# Automatically:

# library(lmtest)
lrtest(hatsF,hatsR)

# Compare to Wald:

wald.test(Sigma=vcov(hats),b=coef(hats),Terms=1:1,H0=0)
