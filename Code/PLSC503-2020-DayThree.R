######################################
# Code for PLSC 503 Day Three
# (Spring 2020)
#
# File created 1/28/2018
#
# File last modified 1/20/2020
######################################
# setwd() first:
#
# setwd("~/PLSC 503/Notes") # or whatever...
#
# Load relevant packages:

library(foreign)
library(RCurl)

# Read in data (from web):

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/CountryData2000.csv")
Data <- read.csv(text = url) # read the country-level data
rm(url)

# Regression:

IMdata<-na.omit(Data[c("infantmortalityperK","DPTpct")])

IMDPT<-lm(infantmortalityperK~DPTpct,data=IMdata,
          na.action=na.exclude)
summary.lm(IMDPT)

# Estimated variance-covariance matrix:

vcov(IMDPT)

# Confidence intervals around the betas:

confint(IMDPT)

# Change the significance level...

confint(IMDPT,level=0.99)

# Predictions w/standard errors:

SEs<-predict(IMDPT,interval="confidence")
SEs

# Plot it!:

Sort<-order(IMdata$DPTpct)
 
pdf("IMDPT-CI.pdf",6,5)
plot(IMdata$DPTpct,IMdata$infantmortalityperK,
     xlab="DPT Immunization Percentage",
     ylab="Infant Mortality Per 1000 Births")
abline(IMDPT,lwd=3)
lines(sort(IMdata$DPTpct),SEs[Sort,2],col="red",lwd=2,lty=2)
lines(sort(IMdata$DPTpct),SEs[Sort,3],col="red",lwd=2,lty=2)
dev.off()

