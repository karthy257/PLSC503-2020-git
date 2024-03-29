####################################################
# PLSC 503 -- Spring 2020: Code for Day Twelve.
####################################################

# install.packages("gvlma") # <- uncomment as needed
library(gvlma)

library(RCurl)
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/flintstones.csv")
flintstones<-read.csv(text=temp, header=TRUE)
rm(temp)

# No Barney OR Dino:
summary(lm(Y~X,data=subset(flintstones,name!="Dino" & name!="Barney")))

# No Barney (Dino included):
summary(lm(Y~X,data=subset(flintstones,name!="Barney")))

# Variance:

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/LittleDahl.csv")
LittleDahl<-read.csv(text=temp, header=TRUE)
rm(temp)

library(car)
with(LittleDahl, scatterplotMatrix(~age+tenure+unified+nulls))

Fit<-with(LittleDahl, lm(nulls~age+tenure+unified))
summary(Fit)

FitResid<-with(LittleDahl, (nulls - predict(Fit))) # residuals
FitStandard<-rstandard(Fit) # standardized residuals
FitStudent<-rstudent(Fit) # studentized residuals
FitCooksD<-cooks.distance(Fit) # Cook’s D
FitDFBeta<-dfbeta(Fit) # DFBeta
FitDFBetaS<-dfbetas(Fit) # DFBetaS
FitCOVRATIO<-covratio(Fit) # COVRATIOs

FitStudent[74]
LittleDahl$Congress74<-rep(0,length=104)
LittleDahl$Congress74[74]<-1
summary(with(LittleDahl, lm(nulls~age+tenure+unified+Congress74)))

influencePlot(Fit,id.n=4,labels=LittleDahl$Congress,id.cex=0.8,
              id.col="red",xlab="Leverage")

dfbetasPlots(Fit,id.n=5,id.col="red",main="",pch=19)

plot(FitCOVRATIO~names(FitCOVRATIO),pch=19,xlab="Congress",
     ylab="Value of COVRATIO")
abline(h=1,lty=2)

Outlier<-rep(0,104)
Outlier[74]<-1
Outlier[98]<-1
Outlier[104]<-1
DahlSmall<-LittleDahl[which(Outlier==0),]

summary(lm(nulls~age+tenure+unified,data=DahlSmall))

########
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/africa2001.csv")
Africa<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(Africa)
rownames(Africa) <- Africa$cabbr

Fit <- with(Africa, 
            lm(adrate~gdppppd+muslperc+subsaharan+healthexp+
                 literacy+internalwar))
summary(Fit)

# What not to do:

library(gvlma)
Nope <- gvlma(Fit)
display.gvlmatests(Nope)

# Better:

pdf("DefaultLMPlots.pdf",10,8)
par(mfrow=c(2,3))
plot(Fit,which=c(1:6),labels.id=rownames(Africa))
dev.off()

# Unpacking that:
#
# #1: Residuals vs. fitted, same as:

pdf("ResidVsFitted.pdf",7,6)
par(mar=c(4,4,2,2))
plot(Fit,which=1,labels.id=rownames(Africa))
dev.off()

# #2: QQ plot of residuals:

pdf("ResidQQPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=2,labels.id=rownames(Africa))
dev.off()

# #3: Scale-Location plot:

pdf("ScaleLocationPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=3,labels.id=rownames(Africa))
dev.off()

# #4: Cook's Distance (D):

pdf("CooksDPlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=4,labels.id=rownames(Africa))
dev.off()


# #5: Residuals vs. Leverage:

pdf("ResidVsLeveragePlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=5,labels.id=rownames(Africa))
dev.off()

# #6: Cook's D vs. Leverage:

pdf("CooksDVsLeveragePlot.pdf",6,5)
par(mar=c(4,4,2,2))
plot(Fit,which=6,labels.id=rownames(Africa))
dev.off()

# Another useful plot:

library(car)

ToPlot<-data.frame(Africa$adrate,Fit$fitted.values,
        Fit$residuals,Africa$gdppppd,Africa$muslperc,
        Africa$subsaharan,Africa$healthexp,Africa$literacy,
        Africa$internalwar)

pdf("UsefulPlot.pdf",8,7)
scatterplotMatrix(ToPlot)
dev.off()
