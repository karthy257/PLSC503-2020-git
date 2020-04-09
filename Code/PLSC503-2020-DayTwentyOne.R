##########################################
# Code for PLSC 503 - Spring 2020
#
# Binary Response Models: Practicum
#
##########################################
# Packages, etc.:

require(RCurl)
library(car)
# install.packages("ROCR") <- as needed
library(ROCR)

# Options:

options(scipen = 6) # bias against scientific notation
options(digits = 3) # show fewer decimal places

#################################
# NAFTA example...

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/NAFTA.csv")
NAFTA<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(NAFTA)

# Probit:

NAFTA.GLM.probit<-glm(vote~democrat+pcthispc+cope93+DemXCOPE,
                   NAFTA,family=binomial(link="probit"))
summary(NAFTA.GLM.probit)

# Logit:

NAFTA.GLM.fit<-glm(vote~democrat+pcthispc+cope93+DemXCOPE,
                   NAFTA,family=binomial)
summary(NAFTA.GLM.fit)

# Compare logit vs. probit estimates:

comparedf <-data.frame(probit = coefficients(NAFTA.GLM.probit),
                       logit = coefficients(NAFTA.GLM.fit))
lpfit<-lm(logit~probit,data=comparedf)

pdf("NAFTAprobitVsLogit.pdf",6,6)
par(mar=c(4,4,2,2))
with(comparedf, 
     plot(probit,logit,pch=20,xlim=c(-0.5,3.5),
          ylim=c(-2,8),xlab="Logit Estimates",
          ylab="Probit Estimates"))
with(comparedf, text(probit,logit,labels=rownames(comparedf),
                     pos=c(1,3,3,1,4)))
abline(lpfit,lwd=2,lty=2,col="red")
text(1,5.5,col="red",labels=paste0("Adj. R-squared = ",
                         round(summary(lpfit)$adj.r.squared,2)))
dev.off()


# Interactions...

NAFTA.GLM.fit$coeff[4]+NAFTA.GLM.fit$coeff[5]
(NAFTA.GLM.fit$coeff[4]+NAFTA.GLM.fit$coeff[5]) / 
  (sqrt(vcov(NAFTA.GLM.fit)[4,4] + 
  (1)^2*vcov(NAFTA.GLM.fit)[5,5] + 
  2*1*vcov(NAFTA.GLM.fit)[4,5]))

# Same thing, using -linear.hypothesis- in -car-:

linear.hypothesis(NAFTA.GLM.fit,"cope93+DemXCOPE=0")

# Predicted values:

preds<-NAFTA.GLM.fit$fitted.values
hats<-predict(NAFTA.GLM.fit,se.fit=TRUE)

# Plotting in-sample predictions:

XBUB<-hats$fit + (1.96*hats$se.fit) 
XBLB<-hats$fit - (1.96*hats$se.fit)
plotdata<-cbind(as.data.frame(hats),XBUB,XBLB)
plotdata<-data.frame(lapply(plotdata,binomial(link="logit")$linkinv))
par(mfrow=c(1,2))
library(plotrix)
plotCI(cope93[democrat==1],plotdata$fit[democrat==1],ui=plotdata$XBUB[democrat==1],
         li=plotdata$XBLB[democrat==1],pch=20,xlab="COPE Score",ylab="Predicted 
         Pr(Pro-NAFTA Vote)")
plotCI(cope93[democrat==0],plotdata$fit[democrat==0],ui=plotdata$XBUB[democrat==0],
         li=plotdata$XBLB[democrat==0],pch=20,xlab="COPE Score",ylab="Predicted 
         Pr(Pro-NAFTA Vote)")

# Plotting Out-of-sample Predictions:

sim.data<-data.frame(pcthispc=mean(nafta$pcthispc),democrat=rep(0:1,101),
                       cope93=seq(from=0,to=100,length.out=101))
sim.data$DemXCOPE<-sim.data$democrat*sim.data$cope93

OutHats<-predict(NAFTA.GLM.fit,se.fit=TRUE,newdata=sim.data)
OutHatsUB<-OutHats$fit+(1.96*OutHats$se.fit)
OutHatsLB<-OutHats$fit-(1.96*OutHats$se.fit)
OutHats<-cbind(as.data.frame(OutHats),OutHatsUB,OutHatsLB)
OutHats<-data.frame(lapply(OutHats,binomial(link="logit")$linkinv))

par(mfrow=c(1,2))
both<-cbind(sim.data,OutHats)
both<-both[order(both$cope93,both$democrat),]

plot(both$cope93[democrat==1],both$fit[democrat==1],t="l",lwd=2,ylim=c(0,1),
       xlab="COPE Score",ylab="Predicted Pr(Pro-NAFTA Vote)")
lines(both$cope93[democrat==1],both$OutHatsUB[democrat==1],lty=2)
lines(both$cope93[democrat==1],both$OutHatsLB[democrat==1],lty=2)
text(locator(1),label="Democrats")

plot(both$cope93[democrat==0],both$fit[democrat==0],t="l",lwd=2,ylim=c(0,1),
       xlab="COPE Score",ylab="Predicted Pr(Pro-NAFTA Vote)")
lines(both$cope93[democrat==0],both$OutHatsUB[democrat==0],lty=2)
lines(both$cope93[democrat==0],both$OutHatsLB[democrat==0],lty=2)
text(locator(1),label="Republicans")

# Odds Ratios:

lreg.or <- function(model)
       {
        coeffs <- coef(summary(NAFTA.GLM.fit))
        lci <- exp(coeffs[ ,1] - 1.96 * coeffs[ ,2])
        or <- exp(coeffs[ ,1])
        uci <- exp(coeffs[ ,1] + 1.96 * coeffs[ ,2])
        lreg.or <- cbind(lci, or, uci)        
        lreg.or
        }

lreg.or(NAFTA.GLM.fit)

####################
# Goodness of fit:

table(NAFTA.GLM.fit$fitted.values>0.5,nafta$vote==1)
chisq.test(NAFTA.GLM.fit$fitted.values>0.5,nafta$vote==1)

# ROC curves, plots, etc. (using -ROCR-):

NAFTA.GLM.logithats<-predict(NAFTA.GLM.fit,
                       type="response")
preds<-prediction(NAFTA.GLM.logithats,NAFTA$vote)
plot(performance(preds,"tpr","fpr"),lwd=2,lty=2,
       col="red",xlab="1 - Specificity",ylab="Sensitivity")
abline(a=0,b=1,lwd=3)

# "Bad" model:

NAFTA.bad<-with(NAFTA,
                glm(vote~pcthispc,family=binomial(link="logit")))
NAFTA.bad.hats<-predict(NAFTA.bad,type="response")
bad.preds<-prediction(NAFTA.bad.hats,NAFTA$vote)
plot(performance(bad.preds,"tpr","fpr"),lwd=2,lty=2,
     col="red",xlab="1 - Specificity",ylab="Sensitivity")
abline(a=0,b=1,lwd=3)

