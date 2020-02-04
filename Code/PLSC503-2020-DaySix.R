################################################
# PLSC 503 -- Spring 2020
#
# Code for Day Six ("Multivariate Regression")
################################################

# setwd() here...
#
# setwd(~/Whatever)
#
################################
# Added variable plot:

library(RCurl)

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/CountryData2000.csv")
Data <- read.csv(text = url) # read the country-level data
rm(url)

Data<-na.omit(Data[c("infantmortalityperK","DPTpct","healthexpGDP")])

fit<-lm(infantmortalityperK~DPTpct,data=Data)
aux<-lm(healthexpGDP~DPTpct,data=Data)

# Plot:

pdf("AVPlot.pdf",7,6)
plot(aux$residuals,fit$residuals,pch=19,
     xlab="Health Expenditures | DPT Rates: Residuals", 
     ylab="Infant Mortality | DPT Rates: Residuals")
abline(lm(fit$residuals~aux$residuals),lwd=3)
dev.off()

# Using avPlots from car:

library(car) # <- install as necessary

fit2<-lm(infantmortalityperK~DPTpct+healthexpGDP,data=Data)
avPlots(fit2,~healthexpGDP)

#####################
# Toy example (N=4):

Y<-c(4,-2,9,-5)
X1<-c(200,120,430,110)
X2<-c(-17,32,-29,25)
data<-cbind(Y,X1,X2)
scatterplotMatrix(data)

cor(data)

fit<-lm(Y~X1+X2)
summary(fit)

# #####################
# # Computation bit...
# 
# options(digits=16)
# options(scipen=99)
# 
# set.seed(7222009)
# Nbig <- 100000000
# one<-rep(1,Nbig)
# X <- rchisq(Nbig,10)
# Z <- 10000 + 5*X + 10*rnorm(Nbig)
# 
# fit<-lm(Z~X)
# fit
# 
# X<-as.matrix(cbind(one,X))
# Z<-as.matrix(Z)
# beta.hat <- qr.solve(t(X) %*% X) %*% (t(X) %*% Z)
# beta.hat
# 
# (fit$coefficients / beta.hat) * 100 
#   # percent difference
