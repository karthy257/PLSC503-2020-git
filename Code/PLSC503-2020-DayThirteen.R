####################################################
# PLSC 503 -- Spring 2020: Code for Day Thirteen
####################################################

library(RCurl)
library(MASS)
library(sem) # <-- install if needed
library(car)

options(scipen = 6) # bias against scientific notation
options(digits = 4) # show fewer decimal places

##### IV Estimation #####

seed<-1337
set.seed(seed)

mu<-c(0,0,0) # <== X, Z, U
Sigma<-matrix(c(1,0.8,0.4,0.8,1,0,0.4,0,1),
              nrow=3,byrow=TRUE) # Cor(X,Y)=0.8, etc.
Vars<- mvrnorm(500,mu,Sigma)
colnames(Vars)<-c("X","Z","U")
Vars<-data.frame(Vars)

Vars$Y<- 1 + Vars$X + Vars$U

pdf("IVSimScatter.pdf",7,6)
par(mar=c(4,4,2,2))
scatterplotMatrix(Vars,pch=20)
dev.off()


OLS<- lm(Y~X,data=Vars)
summary(OLS)

TSLS<-tsls(Y~I(X),data=Vars,instruments=~Z)
summary(TSLS)

##### ``Real data" example... #####

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/IRData.csv")
IRData<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(IRData)

OLSWar<-lm(logdisputes~logtrade+contiguity+capratio,
           data=IRData)
summary(OLSWar)

TwoSLSWar<-tsls(logdisputes~contiguity+capratio+I(logtrade),
                instruments=~contiguity+capratio+IOs,
                data=IRData)
summary(TwoSLSWar)

ITrade<-lm(logtrade~contiguity+IOs+capratio,
           data=IRData)
summary(ITrade)
IVWarByHand<-with(IRData,
                  lm(logdisputes~capratio+contiguity+
                       (ITrade$fitted.values)))
summary(IVWarByHand)

OLSTrade<-lm(logtrade~logdisputes+contiguity+IOs,
             data=IRData)
summary(OLSTrade)

TwoSLSTrade<-tsls(logtrade~contiguity+IOs+I(logdisputes),
                  instruments=~contiguity+capratio+IOs,
                  data=IRData)
summary(TwoSLSTrade)

# Plots of IVs:

IWar<-lm(logdisputes~contiguity+IOs+capratio,
         data=IRData)

pdf("PrettyGoodInstrument.pdf",7,6)
par(mar=c(4,4,2,2))
with(IRData, 
plot(logtrade,ITrade$fitted.values,pch=20,col="black",
     xlab="Actual Values of ln(Trade)",
     ylab="Predicted Values of ln(Trade)"))
abline(a=0,b=1,lwd=2)
dev.off()

pdf("CrappyInstrument.pdf",7,6)
par(mar=c(4,4,2,2))
with(IRdata, 
plot(logdisputes,IWar$fitted.values,pch=20,col="black",
     ylim=c(-1,3),xlab="Actual Values of ln(Disputes)",
     ylab="Predicted Values of ln(Disputes)"))
abline(a=0,b=1,lwd=2)
dev.off()
