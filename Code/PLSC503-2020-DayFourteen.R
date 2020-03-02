################################################
# PLSC 503 -- Spring 2020: Code for Day Fourteen
################################################
# Packages:

library(car)

# Simulate data:

set.seed(7222009)
N <- 100
X1<-rnorm(N)             # <- X1
X2<-(-X1)+1.5*(rnorm(N)) # <- correlated w/X1
Y<-X1-(2*X2)+(2*(rnorm(N))) # <- Y
Z<- (-2*X1) + rnorm(N)  # <- correlated w/X1 but irrelevant
data <- data.frame(Y=Y,X1=X1,X2=X2,Z=Z)

# Scatterplot matrix:

pdf("MisspecificationExampleScatterplotMatrixR.pdf",7,7)
scatterplotMatrix(data)
dev.off()

# "Correct" model:

correct<-lm(Y~X1+X2)
summary(correct)

# "Overspecified" model:

overspec<-lm(Y~X1+X2+Z)
summary(overspec)

# "Underspecified" model:

incorrect<-lm(Y~X1)
summary(incorrect)

# Omitted variable plot:

pdf("MisspecifiedResidualsR.pdf",6,6)
plot(data$X2,incorrect$residuals,pch=20,
     xlab="Omitted Variable (X2)",ylab="Residuals")
abline(lm(incorrect$residuals~data$X2),lwd=2)
abline(h=0,lty=2)
abline(v=0,lty=2)
dev.off()

# /fin