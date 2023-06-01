
#TEXTBOOK: Time Series Analysis with Applications in R ~ J. Cryer & K-s. Chan
#Class projects&assignments

#Model Diagnostics

library(TSA)

#Question 8.5

# Simulate an MA(1) model with n= 36 and θ= −0.5.
# (a)Fit the correctly specified MA(1) model and look at a time series plot of the
# residuals. Does the plot support the MA(1) specification?
# (b)Display a normal quantile-quantile plot of the standardized residuals. Does
# the plot support the MA(1) specification?
# (c) Display the sample ACF of the residuals. Does the plot support the MA(1) specification?
# (d)Calculate the Ljung-Box statistic summing to K= 6. Does this statistic support the 
# MA(1) specification?

set.seed(12345)
ma1.1<-arima.sim(model = list(ma=0.5),n=36)

#part a
#Fit the correctly specified MA(1) model and look at a time series plot of the
#residuals
ma1.fit<-arima(ma1.1,order = c(0,0,1))
plot(residuals(ma1.fit),ylab='Residuals',type='o')
abline(h=0)

#Part b
#Display a normal quantile-quantile plot of the standardized residuals
qqnorm(rstandard(ma1.fit))
qqline(rstandard(ma1.fit))

#part c. Display the sample ACF of the residuals
acf(residuals(ma1.fit))

#Part d. Calculate the Ljung-Box statistic summing to K= 6
LB.test(ma1.fit,lag=6)


#question 8.6

# Simulate an AR(2) model with n= 48, φ1= 1.5, and φ2= −0.75.
# (a)Fit the correctly specified AR(2) model and look at a time series plot of the
# residuals. Does the plot support the AR(2) specification?
# (b)Display a normal quantile-quantile plot of the standardized residuals. Does
# the plot support the AR(2) specification?
# (c) Display the sample ACF of the residuals. Does the plot support the AR(2)
# specification?
# (d)Calculate the Ljung-Box statistic summing to K= 12. Does this statistic support the 
# AR(2) specification?

set.seed(12345)
ar2.1<-arima.sim(model = list(ar=c(1.5,-0.75)),n=48)

#part a. Fit the correctly specified AR(2) model and look at a time series plot 
#of the residuals
ar2.fit<-arima(ar2.1,order = c(2,0,0))
plot(residuals(ar2.fit),ylab='Residuals',type='o')
abline(h=0)

#Part b
#Display a normal quantile-quantile plot of the standardized residuals
qqnorm(rstandard(ar2.fit))
qqline(rstandard(ar2.fit))

#part c. Display the sample ACF of the residuals
acf(residuals(ar2.fit))

#Part d. Calculate the Ljung-Box statistic summing to K= 12
LB.test(ar2.fit,lag=12)

#question 8.7

# Fit an AR(3) model by maximum likelihood to the square root of the hare abundance series 
# (filename hare).
# (a)Plot the sample ACF of the residuals. Comment on the size ofthe correlations.
# (b)Calculate the Ljung-Box statistic summing to K= 9. Does this statistic support the AR(3) 
# specification?
# (c) Perform a runs test on the residuals and comment on the results.
# (d)Display the quantile-quantile normal plot of the residuals. Comment on the
# plot.
# (e) Perform the Shapiro-Wilk test of normality on the residuals.

data(hare)
hare
ar3.1<-sqrt(hare)
ar3.fit<-arima(ar3.1,order = c(3,0,0),method = 'ML')

#part a. Plot the sample ACF of the residuals
acf(residuals(ar3.fit))

#part b.Calculate the Ljung-Box statistic summing to K= 9
LB.test(ar3.fit,lag=9)

#part c. Perform a runs test on the residuals and comment on the results
runs(residuals(ar3.fit))

#part d. Display the quantile-quantile normal plot of the residuals
qqnorm(residuals(ar3.fit))
qqline(residuals(ar3.fit))

#part e. Perform the Shapiro-Wilk test of normality on the residuals
shapiro.test(residuals(ar3.fit))

#Question 8.8

# Consider the oil filter sales data shown in Exhibit 1.8 on page 7. The data are in
# the file named oilfilters.
# (a)Fit an AR(1) model to this series. Is the estimate of the φparameter significantly 
# different from zero statistically?
# (b)Display the sample ACF of the residuals from the AR(1) fitted model. Comment on the display

data("oilfilters")
oilfilters

#part a. Fit an AR(1) model to this series. Is the estimate of the ?? parameter 
#significantly different from zero statistically

ar1.2.fit<-arima(oilfilters,order = c(1,0,0))
ar1.2.fit

#Part b. Display the sample ACF of the residuals from the AR(1) fitted model
acf(as.numeric(residuals(ar1.2.fit)))
