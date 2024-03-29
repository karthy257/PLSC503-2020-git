##########################################
# Code for PLSC 503 - Spring 2020
#
# Unordered Response Models
#
##########################################
# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

# Packages, etc.:

require(RCurl)
require(mlogit)
require(VGAM)
require(aod)
require(car)

# MNL, etc.

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/Election1992small.csv")
nes92<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(nes92)

nes92.mlogit<-vglm(presvote~partyid, multinomial, nes92)
summary(nes92.mlogit)

Bush.nes92.mlogit<-vglm(formula=presvote~partyid, 
                        family=multinomial(refLevel=1),data=nes92) 
summary(Bush.nes92.mlogit)

Clinton.nes92.mlogit<-vglm(formula=presvote~partyid,
                           family=multinomial(refLevel=2),data=nes92)
summary(Clinton.nes92.mlogit)

# CL

colnames(nes92)<-c("caseid","presvote","partyid","FT.Bush","FT.Clinton","FT.Perot")
nes92$PVote<-factor(nes92$presvote,labels=c("Bush","Clinton","Perot"))
head(nes92)

nes92CL<-mlogit.data(nes92,shape="wide",choice="PVote",varying=4:6)
head(nes92CL,6)

# CL regression:

nes92.clogit<-mlogit(PVote~FT|partyid,data=nes92CL)
summary(nes92.clogit)

# Interpretation:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/Election1992.csv")
BigNES92<-read.csv(text=temp, header=TRUE)
rm(temp)


NES.MNL<-vglm(presvote~partyid+age+white+female,data=BigNES92,
              multinomial(refLevel=1))
summaryvglm(NES.MNL)

wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(5,6))
wald.test(b=c(t(coef(NES.MNL))),Sigma=vcov(NES.MNL),Terms=c(1,3,5,7,9))

# Hats

PickBush<-ifelse(fitted.values(NES.MNL)[,1]>fitted.values(NES.MNL)[,2] 
                 & fitted.values(NES.MNL)[,1]>fitted.values(NES.MNL)[,3], 1,0)
PickWJC<-ifelse(fitted.values(NES.MNL)[,2]>fitted.values(NES.MNL)[,1] 
                & fitted.values(NES.MNL)[,2]>fitted.values(NES.MNL)[,3], 2, 0)
PickHRP<-ifelse(fitted.values(NES.MNL)[,3]>fitted.values(NES.MNL)[,1] 
                & fitted.values(NES.MNL)[,3]>fitted.values(NES.MNL)[,2], 3, 0)
OutHat<-PickBush+PickWJC+PickHRP
table(BigNES92$presvote,OutHat)

# Odds ratios:

mnl.or <- function(model) { 
  coeffs <- c(t(coef(NES.MNL))) 
  lci <- exp(coeffs - 1.96 * diag(vcov(NES.MNL))^0.5) 
  or <- exp(coeffs) 
  uci <- exp(coeffs + 1.96* diag(vcov(NES.MNL))^0.5) 
  lreg.or <- cbind(lci, or, uci) 
  lreg.or 
} 

mnl.or(NES.MNL)

# In-sample predictions:

hats<-as.data.frame(fitted.values(NES.MNL))
names(hats)<-c("Bush","Clinton","Perot")
attach(hats)

pdf("InSampleRScatterplotMatrix.pdf",8,7)
spm(~Bush+Clinton+Perot,pch=20,plot.points=TRUE,
    diagonal="histogram",col=c("black","grey"))
dev.off()

pdf("InSampleMNLPredProbsR.pdf",8,6)
par(mfrow=c(1,3))
plot(BigNES92$partyid,Bush,xlab="Party ID")
plot(BigNES92$partyid,Clinton,xlab="Party ID")
plot(BigNES92$partyid,Perot,xlab="Party ID")
par(mfrow=c(1,1))
dev.off()

# CL example:

nes92.clogit<-mlogit(PVote~FT|partyid,data=nes92CL)
summary(nes92.clogit)

# In-sample predictions:

CLhats<-predict(nes92.clogit,nes92CL)

pdf("InSampleCLHatsR.pdf",7,6)
plot(nes92$FT.Bush,CLhats[,1],pch=19,
     col=rgb(100,0,0,100,maxColorValue=255),
     xlab="Feeling Thermometer",
     ylab="Predicted Probability")
points(nes92$FT.Clinton+runif(nrow(CLhats),-1,1),
       CLhats[,2],pch=4,col=rgb(0,0,100,100,maxColorValue=255))
points(nes92$FT.Perot+runif(nrow(CLhats),-1,1),
       CLhats[,3],pch=17,col=rgb(0,100,0,50,maxColorValue=255))
lines(lowess(nes92$FT.Bush,CLhats[,1]),lwd=2,col="red")
lines(lowess(nes92$FT.Clinton,CLhats[,2]),lwd=2,col="blue")
lines(lowess(nes92$FT.Perot,CLhats[,3]),lwd=2,col="darkgreen")
legend("topleft",bty="n",c("Bush","Clinton","Perot"),
       col=c("red","blue","darkgreen"),pch=c(19,4,17))
dev.off()