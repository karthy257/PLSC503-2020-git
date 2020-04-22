#####################################
# PLSC 503 -- Spring 2020
#
# Event Count materials - Day Two
#####################################
# Options:

options(scipen=12)
options(digits=4)
par(mar=c(4,4,2,2))

# Packages:
library(RCurl)
library(psych)
library(MASS)
library(ggplot2)

# setwd():
setwd("~/Dropbox (Personal)/PLSC 503/Notes")

############################################
# CPB Max(Y) figure:

L.CPB <- seq(0.1,10,by=0.1)
MaxY8 <- (-L.CPB) / (0.8-1)
MaxY5 <- (-L.CPB) / (0.5-1)
MaxY2 <- (-L.CPB) / (0.2-1)

pdf("CPBMaxR.pdf",6,5)
par(mfrow=c(1,1))
par(mar=c(4,4,2,2))
plot(L.CPB,MaxY8,t="l",lwd=3,col="black",xlab="Lambda",
     ylab="Maximum Value of Y")
lines(L.CPB,MaxY5,lwd=3,col="red",lty=2)
lines(L.CPB,MaxY2,lwd=3,col="darkgreen",lty=3)
legend("topleft",bty="n",lty=c(1,2,3),lwd=3,
       col=c("black","red","darkgreen"),
       legend=c("Alpha = 0.8","Alpha = 0.5","Alpha = 0.2"))
dev.off()

#######################################
# Simulated Poisson / NB example:
#
# Poisson data (N=400):

N<-400
set.seed(7222009)
X <- runif(N,min=0,max=1)
YPois <- rpois(N,exp(0+1*X))           # Poisson
YNB <- rnbinom(N,size=1,mu=exp(0+1*X)) # NB with theta=1.0

describe(cbind(YPois,YNB))

# Density plots:

pdf("PoisNBDensities.pdf",8,6)
par(mar=c(4,4,2,2))
plot(density(YNB),lwd=2,lty=2,col="red",
     ylim=c(0,0.4),main="",xlab="Y")
lines(density(YPois),lwd=2,lty=1,col="black")
legend("topright",bty="n",lty=c(1,2),col=c("black","red"),
       lwd=c(2,2),legend=c("Poisson","Neg. Bin. (theta=1)"))
dev.off()


# Regressions:

summary(glm(YPois~X,family="poisson")) # Poisson
summary(glm.nb(YPois~X))               # NB

# More regressions:

summary(glm(YNB~X,family="poisson"))  # Poisson
summary(glm.nb(YNB~X))                # NB

####################################
# Now, a bigger/better sim:

Sims <- 250 # (250 sims each)
theta <- seq(0.1,4,by=0.1) # values of theta
diffs<-matrix(nrow=Sims,ncol=length(theta))

set.seed(7222009)
for(j in 1:length(theta)) {
  for(i in 1:Sims) {
    X<-runif(N,min=0,max=1)
    Y<-rnbinom(N,size=theta[j],mu=exp(0+1*X))
    p<-glm(Y~X,family="poisson")
    nb<-glm.nb(Y~X)
    diffs[i,j]<- ((sqrt(vcov(p))[2,2]) / sqrt(vcov(nb))[2,2])*100
  }
}

Dmeans<-colMeans(diffs)
Dmin<-apply(diffs,2,min)
Dmax<-apply(diffs,2,max)

pdf("PoissonSEsWithNB.pdf",7,6)
par(mar=c(4,4,2,2))
plot(theta,Dmeans,xlim=c(0,4),ylim=c(0,100),
     pch=20,ylab="Percentage",xlab=expression(theta))
segments(theta,Dmin,theta,Dmax)
dev.off()


# SCOTUS amici data:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/Amici.csv")
amici<-read.csv(text=temp, header=TRUE)
rm(temp)

describe(amici)

amici.poisson<-glm(namici~term+civlibs,data=amici,
                   family="poisson")
summary(amici.poisson)

Phats<-fitted.values(amici.poisson)
Uhats<-((amici$namici-Phats)^2 - amici$namici) / (Phats * sqrt(2))
summary(lm(Uhats~Phats))


amici.NB<-glm.nb(namici~term+civlibs,data=amici)
summary(amici.NB)

# alpha:

1 / amici.NB$theta

# Coefficient estimates:

cbind(amici.poisson$coefficients,coef(amici.NB))

# Estimated standard errors:

cbind(diag(sqrt(vcov(amici.poisson))),diag(sqrt(vcov(amici.NB))))


# Plot:

pdf("PoissonNBYHatsR.pdf",6,6)
par(mar=c(4,4,2,2))
plot(amici.poisson$fitted.values,amici.NB$fitted.values,pch=20,
     xlab="Poisson",ylab="Negative Binomial",main="",
     xlim=c(0,3),ylim=c(0,3))
abline(a=0,b=1,lwd=1,lty=2)
dev.off()

