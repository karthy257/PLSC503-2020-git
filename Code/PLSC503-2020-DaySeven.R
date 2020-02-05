################################################
# PLSC 503 -- Spring 2020: Code for Day Seven.
################################################

library(RCurl)
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/africa2001.csv")
Data<-read.csv(text=temp, header=TRUE)
Data<-with(Data, data.frame(adrate,polity,
          subsaharan=as.numeric(subsaharan),muslperc,literacy))
summary(Data)
cor(Data)

library(car) # <-- Necessary for linearHypothesis commands, below

scatterplotMatrix(Data)

# Linerar model...

model<-lm(adrate~polity+subsaharan+muslperc+literacy,data=Data)
summary(model)

options(digits=4)
vcov(model)

# Linear hypothesis (F) tests...

library(lmtest)
modelsmall<-lm(adrate~muslperc+literacy,data=Data)
waldtest(model,modelsmall)

library(car)
linearHypothesis(model,"muslperc=0.1")

linearHypothesis(model,"literacy=muslperc")

# Confidence ellipse

confidenceEllipse(model=model,which.coef=c(4,5),
                   xlab="Muslim Percentage",ylab="Literacy")
abline(h=0,v=0,lty=2)

# Predicted values:

hats<-fitted(model)

# Or, alternatively:

fitted<-predict(model,se.fit=TRUE, interval=c("confidence"))

# Plotted:

scatterplot(model$fitted~Data$adrate,log="x",smooth=FALSE,boxplots=FALSE,
           reg.line=FALSE,xlab="Observed HIV Rate",ylab="Predicted HIV Rate",
           pch=16,cex=2)

# Or:

library(plotrix) # <-- install as necessary
plotCI(Data$adrate,model$fitted,uiw=(1.96*(fitted$se.fit)),
       log="x",xlab="Observed HIV Rate",ylab="Predicted HIV Rate")
lines(lowess(Data$adrate,Data$adrate),lwd=2)

