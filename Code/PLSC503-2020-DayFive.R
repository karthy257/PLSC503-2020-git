################################################
# PLSC 503 -- Spring 2020
#
# Code for Day Five: Bootstrapping...
#
################################################
#
# Packages, etc.:

library(RCurl)
library(boot) # <-- install as necessary
library(simpleboot) # <-- install as necessary

# Bootstrapping simulations:

N<-100
reps<-999

set.seed(7222009)
X<-rnorm(N)
Y<-2+2*X+rnorm(N)

# The data:

pdf("BootScatter1.pdf",6,5)
par(mar=c(4,4,2,2))
plot(X,Y,pch=19)
dev.off()

# OLS: 
data<-data.frame(Y,X)
fitOLS<-lm(Y~X)
CI<-confint(fitOLS)

# "By-hand":

B0<-numeric(reps)
B1<-numeric(reps)

for (i in 1:reps) {
  temp<-data[sample(1:N,N,replace=TRUE),]
  temp.lm<-lm(Y~X,data=temp)
  B0[i]<-temp.lm$coefficients[1]
  B1[i]<-temp.lm$coefficients[2]  
}

ByHandB0<-median(B0)
ByHandB1<-median(B1)
ByHandCI.B0<-quantile(B0,probs=c(0.025,0.975)) # <-- 95% c.i.s
ByHandCI.B1<-quantile(B1,probs=c(0.025,0.975))

# Using -boot-:

Bs<-function(formula, data, indices) { # <- regression function
  dat <- data[indices,]
  fit <- lm(formula, data=dat)
  return(coef(fit))
}

Boot.fit<-boot(data=data, statistic=Bs,
               R=reps, formula=Y~X)
BootB0<-median(Boot.fit$t[,1])
BootB1<-median(Boot.fit$t[,2])
BootCI.B0<-boot.ci(Boot.fit,type="basic",index=1)
BootCI.B1<-boot.ci(Boot.fit,type="basic",index=2)


# Using -simpleboot-:

Simple<-lm.boot(fitOLS,reps)
SimpleB0<-perc(Simple,.50)[1]
SimpleB1<-perc(Simple,.50)[2]
Simple.CIs<-perc(Simple,p=c(0.025,0.975))

pdf("BootstrapSims.pdf",7,7)
par(mar=c(4,4,2,2))
plot(c(1,6),c(fitOLS$coefficients[1],fitOLS$coefficients[2]),
     xlim=c(0,10),ylim=c(1.5,2.5),xlab="Parameter",ylab="Estimate",
     xaxt="n",pch=19,col="black")
abline(h=2,lwd=1,lty=2)
axis(1,at=c(2.5,7.5),labels=c("Intercept","Slope"))
points(c(2,7),c(ByHandB0,ByHandB1),pch=4,col="red")
points(c(3,8),c(BootB0,BootB1),pch=17,col="blue")
points(c(4,9),c(SimpleB0,SimpleB1),pch=15,col="darkgreen")
segments(1,CI[1,1],1,CI[1,2],col="black",lwd=2)
segments(6,CI[2,1],6,CI[2,2],col="black",lwd=2)
segments(2,ByHandCI.B0[1],2,ByHandCI.B0[2],col="red",lwd=2)
segments(7,ByHandCI.B1[1],7,ByHandCI.B1[2],col="red",lwd=2)
segments(3,BootCI.B0$basic[4],3,BootCI.B0$basic[5],col="blue",lwd=2)
segments(8,BootCI.B1$basic[4],8,BootCI.B1$basic[5],col="blue",lwd=2)
segments(4,Simple.CIs[1,1],4,Simple.CIs[2,1],col="darkgreen",lwd=2)
segments(9,Simple.CIs[1,2],9,Simple.CIs[2,2],col="darkgreen",lwd=2)
legend("topright",c("OLS","By-Hand","boot","simpleboot"),bty="n",
       pch=c(19,4,17,15),col=c("black","red","blue","darkgreen"))
dev.off()


# Skewed residuals, N=10:

N<-10
reps<-999

set.seed(7222009)
X<-rnorm(N)
ustar<-rchisq(N,1) # <- skewed residuals
Y<-2+2*X+(ustar-mean(ustar))

pdf("BootScatter2.pdf",5,5)
plot(X,Y,pch=19)
dev.off()

data<-data.frame(Y,X)
fitOLS<-lm(Y~X)
CI<-confint(fitOLS)

# Bootstrap SEs and CIs "by hand"

B0<-numeric(reps)
B1<-numeric(reps)

for (i in 1:reps) {
  temp<-data[sample(1:N,N,replace=TRUE),]
  temp.lm<-lm(Y~X,data=temp)
  B0[i]<-temp.lm$coefficients[1]
  B1[i]<-temp.lm$coefficients[2]  
}

ByHandB0<-median(B0)
ByHandB1<-median(B1)
ByHandCI.B0<-quantile(B0,probs=c(0.025,0.975))
ByHandCI.B1<-quantile(B1,probs=c(0.025,0.975))

# Bootstrap SEs and CIs using boot package

library(boot)

Bs<-function(formula, data, indices) { # <- regression function
  dat <- data[indices,]
  fit <- lm(formula, data=dat)
  return(coef(fit)) 
} 

Boot.fit<-boot(data=data, statistic=Bs, 
               R=reps, formula=Y~X)

BootB0<-median(Boot.fit$t[,1])
BootB1<-median(Boot.fit$t[,2])
BootCI.B0<-boot.ci(Boot.fit,type="basic",index=1)
BootCI.B1<-boot.ci(Boot.fit,type="basic",index=2)

# Same, using the simpleboot package

library(simpleboot)
Simple<-lm.boot(fitOLS,reps)
SimpleB0<-perc(Simple,.50)[1]
SimpleB1<-perc(Simple,.50)[2]
Simple.CIs<-perc(Simple,p=c(0.025,0.975))


######################
# Plot:

pdf("BootstrapSims2.pdf",7,7)
par(mar=c(4,4,2,2))
plot(c(1,6),c(fitOLS$coefficients[1],fitOLS$coefficients[2]),
     xlim=c(0,10),ylim=c(-1,3),
     xlab="Parameter",ylab="Estimate",
     xaxt="n",pch=19,col="black")
abline(h=2,lwd=1,lty=2)
axis(1,at=c(2.5,7.5),labels=c("Intercept","Slope"))
points(c(2,7),c(ByHandB0,ByHandB1),pch=4,col="red")
points(c(3,8),c(BootB0,BootB1),pch=17,col="blue")
points(c(4,9),c(SimpleB0,SimpleB1),pch=15,col="darkgreen")
segments(1,CI[1,1],1,CI[1,2],col="black",lwd=2)
segments(6,CI[2,1],6,CI[2,2],col="black",lwd=2)
segments(2,ByHandCI.B0[1],2,ByHandCI.B0[2],col="red",lwd=2)
segments(7,ByHandCI.B1[1],7,ByHandCI.B1[2],col="red",lwd=2)
segments(3,BootCI.B0$basic[4],3,BootCI.B0$basic[5],col="blue",lwd=2)
segments(8,BootCI.B1$basic[4],8,BootCI.B1$basic[5],col="blue",lwd=2)
segments(4,Simple.CIs[1,1],4,Simple.CIs[2,1],col="darkgreen",lwd=2)
segments(9,Simple.CIs[1,2],9,Simple.CIs[2,2],col="darkgreen",lwd=2)
legend("bottomleft",c("OLS","By-Hand","boot","simpleboot"),bty="n",
       pch=c(19,4,17,15),col=c("black","red","blue","darkgreen"))
dev.off()


# Bootstrapping: Justice data...

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/Justices.csv")
Justices<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(Justices)

JOLS<-with(Justices, lm(civrts~score))
JOLShats<-predict(JOLS,interval="confidence")
summary(JOLS)

JBoot <- lm.boot(JOLS, reps)
summary(JBoot)

Sort<-order(Justices$score)

pdf("SCOTUS-bootstrap.pdf",6,5)
par(mar=c(4,4,2,2))
plot(JBoot,xlab="Segal-Cover Score",ylab="Liberal Voting Percent",
     pch=20,lwd=c(3,2,2))
lines(sort(Justices$score), JOLShats[Sort,2],col="red",lty=4,lwd=1)
lines(sort(Justices$score), JOLShats[Sort,3],col="red",lty=4,lwd=1)
legend("topleft",legend=c("OLS","Bootstrap"),col=c("red","black"),
       lty=c(4,2),bty="n")
dev.off()

