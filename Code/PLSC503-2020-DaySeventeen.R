#######################################################
# PLSC 503 - Spring 2020
#
# MLE, Day One code
#######################################################

options(scipen=999)
options(digits=4)
par(mar=c(4,4,2,2))

# Toy example:

data<-c(64,63,59,71,68) # Data
Lis<-dnorm(data,mean=68,sd=4) # Likelihoods for m=68,s=4
L68.4<-prod(Lis) # The likelihood (joint product) for m=68,s=4

Mus<-seq(62,68,by=0.1) # possible values of mu [62,68]
L<-numeric(length(Mus)) # a place to put the likelihoods

# Calculate likelihoods for different values of mu:

for (i in 1:length(Mus)) {
  L[i]<-prod(dnorm(data,mean=Mus[i],sd=4))
}

# Plot:

pdf("SalaryLR.pdf",5,4)
plot(Mus,L,t="l",lwd=2,xlab=expression(hat(mu)),
     ylab="Likelihood")
dev.off()

# Log-L Plot:

lnL<-numeric(length(Mus)) # a place to put the lnLs

for (i in 1:length(Mus)) {
  lnL[i]<-sum(log(dnorm(data,mean=Mus[i],sd=4)))
}

pdf("SalarylnLR.pdf",5,4)
plot(Mus,lnL,t="l",lwd=2,xlab=expression(hat(mu)),
     ylab="Log-Likelihood")
dev.off()

######################################
# Taylor series plot:

x <- seq(-6,6,0.01)
fx <- sin(x)
a <- -1     # pick a value of x at which to locate the approximation...

# recall that the first derivative of sin(x) is cos(x) and 
# vice-versa. So...

TS1 <- sin(a) + (cos(a)/factorial(1))*(x-a) # 1st-order
TS2 <- sin(a) + (cos(a)/factorial(1))*(x-a) - 
  (sin(a)/factorial(2))*((x-a)^2) # 2nd-order
TS3 <- sin(a) + (cos(a)/factorial(1))*(x-a) + 
  (-sin(a)/factorial(2))*((x-a)^2) +
  (-cos(a)/factorial(3))*((x-a)^3) # 3rd-order

# Plot:

pdf("TaylorSeriesIllustrated.pdf",7,6)
par(mar=c(4,4,2,2))
plot(x,fx,t="l",lwd=4,ylim=c(-4,4),
     xlab="x",ylab="f(x) = sin(x)")
points(a,sin(a),pch=19,cex=1.5)
lines(x,TS1,lwd=3,lty=2,col="red")
lines(x,TS2,lwd=3,lty=3,col="blue")
lines(x,TS3,lwd=3,lty=4,col="gold")
#abline(h=0,lwd=1,lty=2)
abline(v=a,lwd=1,lty=2)
text(a-0.5,-4,paste0("a=",a))
legend(a+0.25,-2,bty="n",legend=c("f(x) = sin(x)",
                             "First-Order / Linear",
                             "Second-Order / Quadratic",
                             "Third-Order / Cubic"),
       lwd=c(4,3,3,3),lty=c(1,2,3,4),
       col=c("black","red","blue","gold"))
dev.off()
