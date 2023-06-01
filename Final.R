
#TEXTBOOK: Time Series Analysis with Applications in R ~ J. Cryer & K-s. Chan
#Class projects&assignments

#Comprehensive

library(TSA)

#question 3
 
# Consider the annual rainfall data for Los Angeles shown in Exhibit (1.1),page 2. 
# The quantile-quantile normal plot of these data, shown in Exhibit (3.17), page 50,
# convinced us that the data were not normal. Use command data(larain) to show the data.
# a. Use software to produce a plot similar to Exhibit (5.11), page 102, and determine the 
# 'best' value of λ for a power transformation of the data.
# b. Display a quantile-quantile plot of the transformed data. Are they more normal? Please
# also run the Shapiro-Wilk normality test.
# c. Produce a time series plot of the transformed values.
# d. Use the transformed values to display a plot of Yt
# versus Yt-1 as in Exhibit (1.2), page 2. 
# Should we expect the transformation to change the dependence or lack of dependence in
# the series?

#part a
data("larain")
y<-BoxCox.ar(larain,method = 'ols')
BoxCox.ar()
y

#part b
qqnorm((larain)^.2,main='')
qqline((larain)^.2)
shapiro.test((larain)^.2)

#part c
plot(larain^0.2,type='o')

#part d
plot(y=(larain)^0.2,x=zlag((larain)^0.2))

#Question 4

# Simulate an AR(2) time series of length n = 72 with ϕ1= 0.7 and ϕ2 = −0.4.
# a. Calculate and plot the theoretical autocorrelation function for this model.
# b. Calculate and plot the sample ACF for your simulated series. How well do the values
# and patterns match the theoretical ACF from part (a)?
# c. What are the theoretical partial autocorrelations for this model?
# d. Calculate and plot the sample PACF for your simulated series. How well do the values
# and patterns match the theoretical PACF from part (c)?

set.seed(12345) 
series=arima.sim(n=72,list(ar=c(0.7,-0.4)))
#part a
phi1=0.7
phi2=-0.4
ACF=ARMAacf(ar=c(phi1,phi2),lag.max=10)
plot(y=ACF[-1],x=1:10,xlab='Lag',ylab='ACF',type='h',ylim=c(-0.5,0.5))
abline(h=0)
ACF
#part b

x<-acf(series)
x
#part c

#part d
y<-pacf(series)
y

#question 5

# Simulate an ARMA(1,1) series with ϕ = 0.7, θ = 0.4, and n = 72.
# a. Find the method-of-moments estimates of ϕ and θ.
# b. Find the conditional least squares estimates of ϕ and θ and compare them with part (a).
# c. Find the maximum likelihood estimates of ϕ and θ and compare them with parts (a) and (b).

set.seed(23456)
series=arima.sim(n=72,list(ar=0.7,ma=-0.4))
#part a
acf(series)$acf

#part b
arima(series,order=c(1,0,1),method='CSS')

#part c
arima(series,order=c(1,0,1),method='ML')


#question 6

# Fit an AR(3) model by maximum likelihood to the square root of the hare
# abundance series (dataset hare).
# a. Plot the sample ACF of the residuals. Comment on the size of the correlations.
# b. Calculate the Ljung-Box statistic summing to K = 9. Does this statistic support the
# AR(3) specification?
# c. Display the quantile-quantile normal plot of the residuals. Comment on the plot.
# d. Perform the Shapiro-Wilk test of normality on the residuals.

data(hare)
model=arima(sqrt(hare),order=c(3,0,0))

#part a
acf(residuals(model))
#part b
LB.test(model,lag=9)

#part c
qqnorm(residuals(model))
qqline(residuals(model))

#part d
shapiro.test(residuals(model))


#question 7

# Simulate an IMA(1,1) process with θ = 0.8 and θ0= 0. Simulate 35 values,
# but set aside the last tve values to compare forecasts with actual values.
# a. Using the first 30 values of the series, and the value for the maximum likelihood estimate
# of θ.
# b. Using the estimated model, forecast the next five values of the series. Plot the series
# together with the five forecasts. What is special about the forecasts?
# c. Compare the five forecasts with the actual values that you set aside.
# d. Plot the forecasts together with 95% forecast limits. Do the actual values fall within the
# forecast limits?

set.seed(34567)
series=arima.sim(n=35,list(order=c(0,1,1),ma=-0.8))[-1]
actual=window(series,start=31)
series=window(series,end=30)
              
#part a
model=arima(series,order=c(0,1,1))
model

#part b
result=plot(model,n.ahead=5,col=NULL,pch=19)

#part c
forecast=result$pred
cbind(actual,forecast)

#part d
plot(model,n.ahead=5,pch=19)
points(x=(31:35),y=actual,pch=3)



