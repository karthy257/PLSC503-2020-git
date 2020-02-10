################################################
# PLSC 503 -- Spring 2020: Code for Day Eight.
#
# Dichotomous predictors...
################################################
# setwd() here:
#
# setwd("~/Dropbox (Personal)/PLSC 503/Notes")

# Ugly dummy variable scatterplot:

set.seed(7222009)
X <- rbinom(100,1,0.5)
Y <- 10 + 10*X + (5*rnorm(100))
Dfit <- lm(Y~X)
Dhat <- predict(Dfit)

pdf("UglyDummyScatterplotR.pdf",5,5)  
par(mar=c(4,4,2,2))
plot(X,Y,pch=20,xaxt="n")
axis(1,at=c(0,1))
points(X,Dhat,pch=4,col="red",cex=2)
abline(Dfit,lwd=2,col="red",lty=2)
legend("bottomright",legend="indicates E(Y|X=0/1)",
       bty="n",col="red",pch=4)
dev.off()

library(RCurl)
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/SCOTUS.csv")
SCOTUS<-read.csv(text=temp, header=TRUE)
summary(SCOTUS)

SCOTUS$civil.econ<-SCOTUS$civlibs + SCOTUS$econs

SCOTUS$termdummies<-factor(SCOTUS$term)

fit1<-with(SCOTUS, lm(Namici~civlibs))
summary(fit1)
with(SCOTUS, t.test(Namici~civlibs))

SCOTUS$civlibeffect<-SCOTUS$civlibs
SCOTUS$civlibeffect[SCOTUS$civlibs==0]<-(-1)
fit2<-with(SCOTUS, lm(Namici~SCOTUS$civlibeffect))
summary(fit2)

fit3<-with(SCOTUS, lm(Namici~lctdiss+multlaw+civlibs+
                        econs+constit+lctlib))
summary(fit3)

fit4<-with(SCOTUS, lm(Namici~lctdiss+multlaw+civlibs+
                        econs+constit+lctlib+term))
summary(fit4)

fit5<-with(SCOTUS, lm(Namici~lctdiss+multlaw+civlibs+
                     econs+constit+lctlib+as.factor(term)))
summary(fit5)

# Plot coefficients:

termbetas<-fit5$coefficients[8:39]
SE5<-sqrt(diag(vcov(fit5)))[8:39]
termUBs <- termbetas + 1.96*(SE5)
termLBs <- termbetas - 1.96*(SE5)
term<-seq(1954,1985)

pdf("TermBetasR.pdf",6,5)  
par(mar=c(4,4,2,2))
plot(term,termbetas, xlab="Term",ylab="Estimated Betas",
     pch=19,ylim=c(-0.5,2.1))
lines(term,termbetas,lwd=2)
segments(term,termLBs,term,termUBs,lwd=2,lty=2)
abline(h=0,lwd=2,lty=2,col="red")
dev.off()

