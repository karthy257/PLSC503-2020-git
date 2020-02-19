##################################################
# PLSC 503 -- Spring 2020: Code for Day Eleven.
##################################################

# Africa examples:

library(RCurl)
library(car)
# install.packages("stargazer")
library(stargazer)
# install.packages("MASS")
library(MASS)
temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/africa2001.csv")
Africa<-read.csv(text=temp, header=TRUE)
rm(temp)

# Perfect multicollinearity:

Africa$newgdp<-(Africa$gdppppd-mean(Africa$gdppppd))*1000

fit<-with(Africa, lm(adrate~gdppppd+newgdp+healthexp+subsaharan+
                       muslperc+literacy))
summary(fit)

# N = K

smallAfrica<-subset(Africa,subsaharan=="Not Sub-Saharan")
fit2<-with(smallAfrica,lm(adrate~gdppppd+healthexp+muslperc+
                            literacy+war))
summary(fit2)

# Multicollinearity examples:

with(Africa, table(internalwar,intensity))

HIV1<-with(Africa, lm(adrate~internalwar))
HIV2<-with(Africa, lm(adrate~intensity))
HIV3<-with(Africa, lm(adrate~internalwar+intensity))

stargazer(HIV1,HIV2,HIV3)

##################
# NAFTA example?

temp<-getURL("https://raw.githubusercontent.com/PrisonRodeo/PLSC503-2020-git/master/Data/impeachment.csv")
impeachment<-read.csv(text=temp, header=TRUE)
rm(temp)

summary(impeachment)

fit<-with(impeachment,
          lm(votesum~ADA98+GOPmember+clint96+pctbl96+unionpct))
summary(fit)

idata=impeachment[c(-1,-2)]
cor(idata)

vif(fit)

# Drop GOP...

fit2<-lm(votesum~ADA98+clint96+pctbl96+unionpct)
summary(fit2)

vif(fit2)

# Ridge regression:

ridge.vote<-lm.ridge(votesum~ADA98+GOPmember+clint96+pctbl96+unionpct,
                     data=impeachment,lambda=seq(1,5001,10))
select(ridge.vote)

# Plot:

pdf("RidgePlotInR.pdf",6,5)
par(mar=c(4,4,2,2))
matplot(ridge.vote$lambda,t(ridge.vote$coef),type="l",
        xlab=paste("log(lambda)"),
        ylab=expression(hat(beta)),lwd=2,log="x")
abline(h=0)
abline(v=10)
dev.off()

