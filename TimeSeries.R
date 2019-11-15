# install.packages("fBasics")  # basic statistics 
# install.packages("forecast")  # time series and test for stationarity

library(fBasics)
library(forecast) 
library(dplyr)
# ******************************************
# *** SESSION 3. Real Example ***
# ******************************************
dataset<-read.csv("C:/Users/Chiara SIMONETTI/PERSONAL/MDB Docs/14-Time Series/Assignment 1/Homework_1_DATA.csv",header=TRUE,sep=";",dec=",")
View(dataset)

#########################################################
##SERIES 1
series1<-dataset[,1][1:300]      # the data is available up to row 300
y <- series1

# achieving stationarity and identifying the model
# Is my data stationary? Do I have to take any differences or logs?
par(mar=c(2,2,2,2))

ts.plot(y)  
par(mfrow=c(2,1))
acf(y)  
pacf(y)

ndiffs(y, alpha=0.05, test=c("adf"))

# adfTest(y,lags=10,type=c("c"))

mean(y) # compute basic statistics
sd(y)
skewness(y)
kurtosis(y,method=c("moment"))  

##verify white noise
Box.test(y,lag=12)
Box.test(y,lag=20)

##verify strict white noise
par(mfrow=c(3,1))
ts.plot(y^2)
acf(y^2)
pacf(y^2)    

Box.test(y^2,lag=7)
Box.test(y^2,lag=10)
Box.test(y^2,lag=20)

# testing for normality 
shapiro.test(y)  # 95% confidence intervals are robust for any kind of distribution

hist(y,prob=T,ylim=c(0,0.5),xlim=c(mean(y)-3*sd(y),mean(y)+3*sd(y)),col="red")
lines(density(y),lwd=2)
mu<-mean(y)
sigma<-sd(y)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")

sd(y)

quantile(y, probs=c(0.025,0.975)) # 95% confidence interval
# -1.96*sd(fit$residuals)

#########################################################
## SERIES 2
## subsetting the series 2
series2 <-dataset[,2]
View(series2)


y2 <- series2

# Verifying stationarity - the plot confirmes that the series is not stationary
ts.plot(y2)  

par(mfrow=c(2,1))
ts.plot(y2)  
acf(y2)  
pacf(y2)

mean(y2) # compute basic statistics
sd(y2)
skewness(y2)
kurtosis(y2)  

## the Augmented Dickey Fuller test required 1 diff for stationarity
ndiffs(y2, alpha=0.05, test=c("adf"))

#The original series is clearly non-stationary:
# 1.	The time series plot says that the level is not constant over time
# 2.	The acf decays very slowly to zero
# 3.	The adf test states the need of at least one unit root


# first difference
z2<-diff(y2)


par(mfrow=c(3,1))
ts.pl2ot(z2) 
acf(z)  
pacf(z2)

mean(z2) #mean is -0.03 statistically insignificant?
sd(z2)
skewness(z2)
kurtosis(z2)  

ndiffs(z2, alpha=0.05, test=c("adf")) #ndiffs is zero 


#Checking for normality graphically
hist(z2,prob=T,ylim=c(0,0.5),xlim=c(mean(z2)-3*sd(z2),mean(z2)+3*sd(z2)),col="red")
lines(density(z2),lwd=2)
mu<-mean(z2)
sigma<-sd(z2)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")


#B.	Testing for WHITE NOISE graphically
par(mfrow=c(3,1))
ts.plot(z2)   
acf(z2)
pacf(z2)



# formal test for white noise (zero autocorrelations)
# Ho: uncorrelated data
# H1: correlated data
Box.test (z, lag = 20, type="Ljung")  # Null: ro1=.=ro20=0

#p-value 0.92 > 0.05 

# C.	Testing for STRICT WHITE NOISE
par(mfrow=c(3,1)) # analysis of the squared data
ts.plot(z2^2)   
acf(z2^2)
pacf(z2^2)


# Sometimes we will need to do the same for the transformed data "z"
# formal test for white noise (zero autocorrelations)
# Ho: uncorrelated data
# H1: correlated data
Box.test(z^2,lag=20, type="Ljung")    # Null: ro1=.=ro20=0

# D.	Testing for GAUSSIAN WHITE NOISE
shapiro.test(z2)
# GWN ??? SWN


# formal normality test
# Ho: the data is normally distributed
# H1: the data is not normally distributed
shapiro.test(z2)


#########################################################
##SERIES 3
series3=dataset[,3] [1:300]
y3 <- series3[1:300]#leaving 6 for predictions
## we remove the last 6 points to evaluate the prediction
par(mar=c(1,1,1,1)) # to adjust graphic size
ts.plot(y3)   #as we can see that the Series is not stataionty in the mean, nor in the variance
acf(y3)#all of the Lags are out limit and dont fall under the 95% interval the acf decays very slowly to zero
pacf(y3)#only one lag is out of limit which is lag (1)  

mean(y3) # compute basic statistics
sd(y3)
skewness(y3)
kurtosis(y3) 
## the Augmented Dickey Fuller test required 1 diff for stationarity
ndiffs(y3, alpha=0.05, test=c("adf"))

# first difference
z<-diff(y3)


par(mfrow=c(3,1))
ts.plot(z) 
acf(z)  
pacf(z)

mean(z) #mean is -0.03 statistically insignificant?
sd(z)
skewness(z)
kurtosis(z)  

ndiffs(z, alpha=0.05, test=c("adf")) #ndiffs is zero 


Box.test(z,lag=20)

# already white noise?
# Do we need a non-linear model (quadratic)?

par(mfrow=c(3,1))
ts.plot(z^2)
acf(z^2)
pacf(z^2)    

Box.test(z^2,lag=10)
Box.test(z^2,lag=15)
Box.test(z^2,lag=20)

# testing for normality 
shapiro.test(z)  # 95% confidence intervals are robust for any kind of distribution

hist(z,prob=T,ylim=c(0,0.5),xlim=c(mean(z)-3*sd(z),mean(z)+3*sd(z)),col="red")
lines(density(z),lwd=2)
mu<-mean(z)
sigma<-sd(z)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")
sd(z)
quantile(z, probs=c(0.025,0.975)) # 95% confidence interval
# -1.96*sd(fit$residuals)



#########################################################
##SERIES 4
## select Series 4
series4 <-dataset[,4]
View(series4)
y4 <-series4


## we check how many points are for prediction
na_count <-sum(is.na(y4))
na_count
length(y4)

## we take out the last points with NA´s

y4<-y4[!is.na(y4)]

# achieving stationarity and identifying the model

par(mar=c(1,2,1,1)) # to adjust graphic size
par(mfrow=c(3,1))
ts.plot(y4)# plot the series: it is not stationary in the mean
acf(y4)  # all the lags from 0 to 25 out of limits
pacf(y4) # lag 1 is the only lag out of limits

ndiffs(y4, alpha=0.05, test=c("adf")) # Equal to 2, no need to do any transformation. Data is stationary

### basic statistics
library(psych)


mean(y4) # compute basic statistics
sd(y4)
skewness(y4)
kurtosis(y4) 

#-	Mean: 1256.843
#-	Standard deviation: 1154.857
#-	Skewness: -0.1488262
#-	Kurtosis: -1.440023

# According to the Dickey Fuller test (ndiff function), we need to perform two transformations to make our series stationary

# First difference

z4<-diff(y4)

mean(z4) # compute basic statistics
sd(z4)
skewness(z4)
kurtosis(z4) 

#-	Mean: 11.17006
#-	Standard deviation: 13.82722
#-	Skewness: -0.5206655
#-	Kurtosis: -0.3745931



ts.plot(z4)  
par(mfrow=c(2,1))
acf(z4)  
pacf(z4)

ndiffs(z4, alpha=0.05, test=c("adf"))

# second difference
zz4<-diff(z4)

mean(zz4) # compute basic statistics
sd(zz4)
skewness(zz4)
kurtosis(zz4) 

#-	Mean: 0.1287819
#-	Standard deviation: 1.445643
#-	Skewness: 0.1289298
#-	Kurtosis: -0.2381873

ts.plot(zz4)  

par(mfrow=c(2,1))
acf(zz4)  
pacf(zz4)

ndiffs(zz4, alpha=0.05, test=c("adf"))

##iS IT WHITE NOISE?
Box.test(zz4,lag=20) # p-value > 0.05 => no correlation


# therefore we can concluse its not WN as we have lags out of limits, there´s a linear relationship and no idenpendece, therefore it´s not SWN and then not GWN

## Confirm it´s not normally distributed
shapiro.test(zz4) ## p_value is lower than 0.05 then we confirm it´s not normally distributed
par(mar=c(3,3,3,3))
par(mfrow=c(1.5,1))
hist(zz,prob=T,ylim=c(0,0.6),xlim=c(mean(zz4)-4*sd(zz4),mean(zz4)+4*sd(zz4)),col="red")
lines(density(zz4),lwd=2)
mu<-mean(zz4)
sigma<-sd(zz4)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)

quantile(zz4, probs=c(0.025,0.975))
sd(zz4)


##verify strict white noise
par(mfrow=c(3,1))
ts.plot(zz4^2)
acf(zz4^2)
pacf(zz4^2)    

Box.test(zz4^2,lag=10)
Box.test(zz4^2,lag=15)
Box.test(zz4^2,lag=20)


#########################################################
##SERIES 5
series5<-dataset[,5][1:2000]
y5 <- series5

par(mar=c(1,2,1,1)) # to adjust graphic size
par(mfrow=c(3,1))
ts.plot(y5)# plot the series: it is not stationary in the mean
acf(y5)  # all the lags from 0 to 25 out of limits
pacf(y5) # lag 1 is the only lag out of limits

ndiffs(y5, alpha=0.05, test=c("adf")) # Equal to 2, no need to do any transformation. Data is stationary

### basic statistics
library(psych)
mean(y5) # compute basic statistics
sd(y5)
skewness(y5)
kurtosis(y5)


##iS IT WHITE NOISE?
Box.test(y5,lag=30) 

##Normal distribution
shapiro.test(y5)

hist(y5,prob=T,ylim=c(0,0.8),xlim=c(mean(y5)-4*sd(y5),mean(y5)+4*sd(y5)),col="red")
lines(density(y5),lwd=2)
mu<-mean(y5)
sigma<-sd(y5)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")

quantile(y5, probs=c(0.025,0.975))
sd(y5)

##strict White noise
par(mfrow=c(3,1))
ts.plot(y5^2)
acf(y5^2)
pacf(y5^2)    

Box.test(y5^2,lag=15)
Box.test(y5^2,lag=20)
Box.test(y5^2,lag=30)

#########################################################
##SERIES 6
series6<-dataset[,6][1:3000]
View(series6)# the data is available up to row 300
y6 <- series6

# achieving stationarity and identifying the model
# Is my data stationary? Do I have to take any differences or logs?
par(mar=c(2,2,2,2))

ts.plot(y6)  
par(mfrow=c(2,1))
acf(y6)  
pacf(y6)

ndiffs(y6, alpha=0.05, test=c("adf"))

# adfTest(y,lags=10,type=c("c"))

mean(y6) # compute basic statistics
sd(y6)
skewness(y6)
kurtosis(y6,method=c("moment"))  

##iS IT WHITE NOISE?
Box.test(y6,lag=30)

Box.test(y6,lag=20)

Box.test(y6,lag=10)

Box.test(y5,lag=8)

#Normal distribution
shapiro.test(y6)

hist(y6,prob=T,ylim=c(0,0.8),xlim=c(mean(y6)-4*sd(y6),mean(y6)+4*sd(y6)),col="red")
lines(density(y6),lwd=2)
mu<-mean(y6)
sigma<-sd(y6)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")

quantile(y6, probs=c(0.025,0.975))
sd(y6)

##strict White noise
par(mfrow=c(3,1))
ts.plot(y6^2)
acf(y6^2)
pacf(y6^2)    

Box.test(y6^2,lag=15)
Box.test(y6^2,lag=20)
Box.test(y6^2,lag=30)

#########################################################
##SERIES 7
series7<-dataset[,7][1:3000]
View(series7)# the data is available up to row 300
y7 <- series7

# achieving stationarity and identifying the model
# Is my data stationary? Do I have to take any differences or logs?
par(mar=c(2,2,2,2))

ts.plot(z7)  
par(mfrow=c(2,1))
acf(y7)  
pacf(y7)

ndiffs(y7, alpha=0.05, test=c("adf"))

# adfTest(y,lags=10,type=c("c"))

mean(y7) # compute basic statistics
sd(y7)
skewness(y7)
kurtosis(y7,method=c("moment"))  

z7 <- diff(y7)

ts.plot(z7)  
par(mfrow=c(2,1))
acf(z7)  
pacf(z7)

ndiffs(z7, alpha=0.05, test=c("adf"))



##iS IT WHITE NOISE?
Box.test(z7,lag=30)

Box.test(z7,lag=20)

Box.test(z7,lag=25)

Box.test(z7,lag=15)


#Normal distribution
shapiro.test(z7)

hist(z7,prob=T,ylim=c(0,0.8),xlim=c(mean(z7)-4*sd(z7),mean(z7)+4*sd(z7)),col="red")
lines(density(z7),lwd=2)
mu<-mean(z7)
sigma<-sd(z7)
x<-seq(mu-3*sigma,mu+3*sigma,length=100)
yy<-dnorm(x,mu,sigma)
lines(x,yy,lwd=2,col="blue")

quantile(z7, probs=c(0.025,0.975))
sd(z7)

##strict White noise
par(mfrow=c(3,1))
ts.plot(z7^2)
acf(z7^2)
pacf(z7^2)    

Box.test(z7^2,lag=15)
Box.test(z7^2,lag=20)
Box.test(z7^2,lag=30)