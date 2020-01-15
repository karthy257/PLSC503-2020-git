######################################
# Code for PLSC 503 Day Two
# (Spring 2020)
#
# File created 1/19/2017
#
# File last modified 1/15/2020
######################################
# setwd() first:
#
# setwd("~/PLSC 503/Notes") # or whatever...
#
# Load relevant packages:

library(RCurl)

# Read in data (from web):

url <- getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/CountryData2000.csv")
Data <- read.csv(text = url) # read the data
rm(url)

# Summary statistics

# install.packages("psych") <- Install psych package, if necessary
library(psych)
with(Data, describe(infantmortalityperK))
with(Data, describe(DPTpct))

# Regression:

IMDPT<-lm(infantmortalityperK~DPTpct,data=Data,na.action=na.exclude)
summary.lm(IMDPT)

# ANOVA

anova(IMDPT)

# Scatterplot:

pdf("IMDPT.pdf",6,6) # <- create PDF
with(Data, plot(DPTpct,infantmortalityperK,pch=".",
     xlab="DPT Immunization Percentage",
     ylab="Infant Mortality (Deaths per 1000 Births)"))
with(Data, text(DPTpct,infantmortalityperK,labels=WBcode))
with(Data, abline(v=mean(DPTpct,na.rm=TRUE),lty=2))
with(Data, abline(h=mean(infantmortalityperK,na.rm=TRUE),lty=2))
abline(IMDPT,lwd=3)
dev.off()

# Residuals (u):

Data$IMDPTres <- with(Data, residuals(IMDPT))
describe(Data$IMDPTres)

# Residual density plot:

pdf("IMDPTResidualsDensity.pdf",6,6)
with(Data, plot(density(IMDPTres,na.rm=TRUE),
                main="Density Plot: Regression Residuals",
                xlab="Residual Value"))
abline(v=0,lty=2,lwd=2)
dev.off()

# Fitted Values:

Data$IMDPThat<-fitted.values(IMDPT)
describe(Data$IMDPThat)

# Densities plot:

pdf("IMDPTFittedDensity.pdf",6,6)
with(Data, plot(density(IMDPThat,na.rm=TRUE),
                main="Density Plot: Actual and Fitted Values",
                xlab="Values of Y"))
with(Data, lines(density(infantmortalityperK,na.rm=TRUE),
                lty=2,col="red"))
with(Data, abline(v=mean(infantmortalityperK,na.rm=TRUE),
                  lty=2,lwd=2))
dev.off()

# Correlations:

with(Data, cor(infantmortalityperK,DPTpct,use="complete.obs"))
with(Data, cor(IMDPTres,infantmortalityperK,use="complete.obs"))
with(Data, cor(IMDPTres,DPTpct,use="complete.obs"))
with(Data, cor(IMDPThat,infantmortalityperK,use="complete.obs"))
with(Data, cor(IMDPThat,DPTpct,use="complete.obs"))
with(Data, cor(IMDPTres,IMDPThat,use="complete.obs"))

# Plotting residuals vs. X:

pdf("IMDPTResiduals.pdf",6,6)
with(Data, plot(DPTpct,IMDPTres,pch=".",
                xlab="DPT Immunization Percentage",
                ylab="Residuals (Y - Y-hat)"))
with(Data, text(DPTpct,IMDPTres,labels=WBcode))
abline(h=0,lwd=2)
dev.off()

# Squared residuals vs. X:

pdf("IMDPTSquaredResiduals.pdf",6,6)
with(Data, plot(DPTpct,IMDPTres^2,pch=".",
                xlab="DPT Immunization Percentage",
                ylab="Residuals (Y - Y-hat)"))
with(Data, text(DPTpct,IMDPTres^2,labels=WBcode))
dev.off() 
