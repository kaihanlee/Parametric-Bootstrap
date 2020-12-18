library(MASS)
library(boot)

#Q1#########################################
#import claim data
claimsstring = scan("TSA_claims.txt",what = c(Claims=""),quote=NULL)
#convert to integers
claims = as.numeric(claimsstring[2:length(claimsstring)]) ; claims

par(mfrow=c(1,2))
#plot histogram
hist(claims,freq=T,xlim=c(min(claims)-1,max(claims)+1),col="blue",main="Histogram of TSA claims",xlab="Claims",ylab="Number of Airports")
#plot boxplot
boxplot(claims,main="Boxplot of TSA claims",xlab="Frequency",ylab="Airports",col="blue",horizontal = T)
summary(claims)   #display summary

#Q2#########################################
#QQ plot of claims versus exponential distribution
plot(sort(claims)~qexp(ppoints(length(claims))),main="QQ plot of TSA claims versus Exponential Distribution",
     xlab="Theoretical Quantities",ylab="Sample Quantities")
slope = lqs(sort(claims) ~ qexp(ppoints(length(claims)))-1)   #parameter of slope
abline(0,slope$coefficients,col="red",lty=2)    #plot slope

#Q4#########################################
#QQ plot of claims versus Pareto distribution
plot(log(sort(claims))~qexp(ppoints(length(claims))),main="QQ plot of TSA claims versus Pareto Distribution",
     xlab="Theoretical Quantities",ylab="Sample Quantities")
slope1 = lqs(log(sort(claims)) ~ qexp(ppoints(length(claims)))-1)   #parameter of slope
abline(0,slope1$coefficients,col="red",lty=2)   #plot slope
alpha = 1/slope1$coefficients     #extract value of alpha

#Q5#########################################
A = 10000
bootmed = numeric(A)    #empty vector
#non-parametric bootstrap to obtain median
for(i in 1:A){
  bootmed[i] = median(sample(claims,size=length(claims),replace=T))
}
summary(bootmed)    #summary of non-parametric bootstrap median

hist(bootmed,col="blue",main="Histogram of Non-Parametric Bootstrap Median",xlab="Bootstrap Median",ylab="Frequency")   #plot histogram
quantile(bootmed,c(0.025,0.975))    #determine 95% quantile
abline(v=quantile(bootmed,c(0.025,0.975)))  #draw 95% confidence interval lines
boxplot(bootmed,col="blue",main="Boxplot of Non-Parametric Bootstrap Median",xlab="Bootstrap Median",horizontal=T)   #plot boxplot

#Q6#########################################
#function of Pareto distribution
pareto = function(x){
  fx=alpha/(x^(alpha+1))
  return(fx)
}

popmed = 2^(1/alpha) ; popmed     #value of m
varmed = (1/(2*pareto(popmed)*sqrt(length(claims))))^2 ; varmed   #value of variance

left = popmed + qnorm(0.975)*sqrt(varmed) ; left     #upper limit of 95% confidence interval
right = popmed - qnorm(0.975)*sqrt(varmed) ; right   #lower limit of 95% confidence interval
conf.int = c(left,right)
names(conf.int) = c("Upper","Lower")
conf.int    #display confidence interval

#Q7########################################
med.hyp = 5     #null hypothesis
alpha0=log(2)/log(med.hyp)  #value of alpha when m=5
bootmed0 = numeric(A)   #empty vector
#parametric bootstrap to obtain median
for (i in 1: A){
  bootmed0[i] = median(exp(rexp(298, rate = alpha0)))
}
summary(bootmed0)   #summary of parametric bootstrap median

bootstrapCI = quantile(bootmed0,c(0.025,0.975))
bootstrapCI

par(mfrow=c(1,2))
# Obtain sampling distribution under H0 and p-value
hist(bootmed0, main="Histogram of Parametric Bootstrap Median",xlab="Bootstrap Median",col="steelblue1")  #plot histogram for parametric bootstrap median
abline(v = median(bootmed), col="red", lwd=2.0)
boxplot(bootmed0, main="Boxplot of Parametric Bootstrap Median", xlab="Bootstrap Median" ,col="steelblue1", horizontal=TRUE)   #plot boxplot

#p-value for median number of claims less than sample median
boot.p = (1+length(bootmed0[bootmed0 < median(bootmed)])) / (A + 1) ; boot.p
