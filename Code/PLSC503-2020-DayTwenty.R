######################################################
# R code for PLSC 503 (2020), Day Twenty
#
# Binary response models, part I
######################################################

# setwd() as necessary

set.seed(7222009)
ystar<-rnorm(100)
y<-ifelse(ystar>0,1,0)
x<-ystar+(0.5*rnorm(100))
data<-data.frame(ystar,y,x)
head(data)

pdf("YstarYX-R.pdf",6,5)
par(mar=c(4,4,2,2))
plot(x,ystar,pch=19,ylab="Y* / Y",xlab="X")
points(x,y,pch=4,col="red")
abline(h=0)
legend("topleft",bty="n",pch=c(19,4),col=c("black","red"),
       legend=c("Y*","Y"))
dev.off()

# probits and logits...

myprobit<-glm(y~x,family=binomial(link="probit"),
              data=data)
summary(myprobit)

mylogit<-glm(y~x,family=binomial(link="logit"),
             data=data)
summary(mylogit)

pdf("LogitProbitHats.pdf",6,5)
plot(mylogit$fitted.values,myprobit$fitted.values,
     pch=20,xlab="Logit Predictions",
     ylab="Probit Predictions")
dev.off()
