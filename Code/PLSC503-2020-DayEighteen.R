####################################################
# PLSC 503 -- Spring 2020: Code for Day Eighteen.
####################################################

library(maxLik,distr)

set.seed(7222009)
U<-runif(100)
rayleigh<-3*sqrt(-2*log(1-U))
loglike <- function(param) {
      b <- param[1]
      ll <- (log(x)-log(b^2)) + ((-x^2)/(2*b^2))
      ll
    }

# Fit...

x<-rayleigh
hatsNR <- maxLik(loglike, start=c(1))
summary(hatsNR)

# Comparing optimizers:

hatsBFGS <- maxLik(loglike, start=c(1),method="BFGS") # BFGS
hatsBHHH <- maxLik(loglike, start=c(1),method="BHHH") # BHHH
labels<-c("Newton-Raphson","BFGS","BHHH")

hats<-c(hatsNR$estimate,hatsBFGS$estimate,hatsBHHH$estimate) # Estimates
ses<-c(sqrt(-(1/(hatsNR$hessian))),sqrt(-(1/(hatsBFGS$hessian))),sqrt(-(1/(hatsBHHH$hessian)))) # SEs
its<-c(hatsNR$iterations,hatsBFGS$iterations,hatsBHHH$iterations) # Iterations

par(mfrow=c(3,1))
dotchart(hats,labels=labels,groups=NULL,pch=16,xlab="Estimate")
dotchart(ses,labels=labels,pch=16,xlab="Standard Error")
dotchart(its,labels=labels,pch=16,xlab="Iterations")

# Bad juju:

Y<-c(0,0,0,0,0,1,1,1,1,1)
X<-c(0,1,0,1,0,1,1,1,1,1) # No obs. where Y=1 and X=0
logL <- function(param) {
     b0<-param[1]
     b1<-param[2]
     ll<-Y*log(exp(b0+b1*X)/(1+exp(b0+b1*X))) + 
         (1-Y)*log(1-(exp(b0+b1*X)/(1+exp(b0+b1*X)))) # lnL for binary logit
     ll
   } # Logit regression function

Bhat<-maxLik(logL,start=c(0,0))
summary(Bhat)

