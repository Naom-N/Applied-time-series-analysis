
#TEXTBOOK: Time Series Analysis with Applications in R ~ J. Cryer & K-s. Chan
#Class projects&assignments

#Forecasting

library(TSA)

#question 9.3

# Using the estimated cosine trend on page 192:
# (a)Forecast the average monthly temperature in Dubuque, Iowa, for April 1976. 
# (b)Find a 95% prediction interval for that April forecast. (The estimate of 
# for this model is 3.719°F.)
# (c) What is the forecast for April, 1977? For April 2009?

data("tempdub")
tempdub
#Forecast the average monthly temperature in Dubuque, Iowa, for April 1976
t=3/12
46.2660+((-26.7079)*cos(2*22/7*t))+((-2.1697)*sin(2*22/7*t))

#Question 9.9

# Consider the monthly electricity generation time series shown in Exhibit 5.8 on
# page 99. The data are in the file named electricity.
# (a)Fit a deterministic trend model containingseasonal means together with a linear time trend 
#to the logarithms of the electricity values.
# (b)Plot the last five years of the series together with two years of forecasts and
# the 95% forecast limits.Interpret the plot

#Simulate an AR(1) process with ??= 0.8 and ??= 100.
set.seed(12345)
ar1<-arima.sim(model = list(ar=0.8),n=48)+100
ar.aside1<-window(ar1,start=41)
ar1<-window(ar1,end=40)

#Part a
#Using the first 40 values of the series, find the values for the maximum 
#likelihood #estimates of ??and ??.
ar1.fit<-arima(ar1,order = c(1,0,0),method = 'ML')
ar1.fit

#part b
plot(ar1.fit,n.ahead=8,type='b')
abline(h=coef(ar1.fit)[names(coef(ar1.fit))=='intercept'])

#part c
#Compare the eight forecasts with the actual values that you set aside
ar.aside1
#and
forecast=plot(ar1.fit,n.ahead=8,type='b')$pred
cbind(ar.aside1,forecast)

#part d
#Plot the forecasts together with 95% forecast limits
plot(ar1.fit,n.ahead=8,type='b')
points(x=41:48,y=ar.aside1,pch=19)
abline(h=coef(ar1.fit)[names(coef(ar1.fit))=='intercept'])

#part e
#Repeat parts (a) through (d) with a new simulated series
ar1<-arima.sim(model = list(ar=0.8),n=48)+100
ar.aside1<-window(ar1,start=41)
ar1<-window(ar1,end=40)
ar1.fit<-arima(ar1,order = c(1,0,0),method = 'ML')
ar1.fit
plot(ar1.fit,n.ahead=8,type='b')
abline(h=coef(ar1.fit)[names(coef(ar1.fit))=='intercept'])
plot(ar1.fit,n.ahead=8,type='b')
points(x=(41:48),y=ar.aside1,pch=3)
abline(h=coef(ar1.fit)[names(coef(ar1.fit))=='intercept'])


#Question 9.12

# Simulate an MA(2) process with θ1= 1, θ2= −0.6, and μ= 100. Simulate 36 values but set 
# aside the last 4 values with compare forecasts to actual values.
# (a)Using the first 32 values of the series, find the values for the maximum likelihood 
# estimates of the θ’s and μ.
# (b)Using the estimated model, forecast the next four values of the series. Plot the series 
# together with the four forecasts. Place a horizontal line at the estimate of the process mean.
# (c) What is special about the forecasts at lead times 3 and 4?
# (d)Compare the four forecasts with the actual values that you set aside.
# (e) Plot the forecasts together with 95% forecast limits. Do the actual values fall
# within the forecast limits?
# (f) Repeat parts (a) through (e) with a new simulated series using the same values
# of the parameters and same sample size.

#Simulate an MA(2) process with ??1= 1, ??2= ???0.6, and ??= 100. 
#Simulate 36, set aside 4.
set.seed(12345)
ma2<-arima.sim(model = list(ma=c(-1,0.6)),n=36)+100
ma2.aside1<-window(ma2,start=33)
ma2<-window(ar1,end=32)

#part a
#Using the first 32 values of the series, find mle estimates of ??'s and ??
ma2.fit<-arima(ma2,order = c(0,0,2),method = 'ML')
ma2.fit

#part b
# Forecast the next four values of the series
plot(ma2.fit,n.ahead=4,type='b')
abline(h=coef(ma2.fit)[names(coef(ma2.fit))=='intercept'])

#Part c and d
#Compare the four forecasts with the actual values that you set aside
ma2.aside1
#and
forecast=plot(ma2.fit,n.ahead=4,type='b')$pred
cbind(ma2.aside1,forecast)

#part e
#Plot the forecasts together with 95% forecast limits
plot(ma2.fit,n.ahead=4,type='b')
points(x=(33:36),y=ma2.aside1,pch=19)
abline(h=coef(ma2.fit)[names(coef(ma2.fit))=='intercept'])

#part f
#repeart the above process
ma2.fit<-arima(ma2,order = c(0,0,2),method = 'ML')
ma2.fit
plot(ma2.fit,n.ahead=4,type='b')
abline(h=coef(ma2.fit)[names(coef(ma2.fit))=='intercept'])
forecast=plot(ma2.fit,n.ahead=4,type='b')$pred
cbind(ma2.aside1,forecast)
plot(ma2.fit,n.ahead=4,type='b')
points(x=(33:36),y=ma2.aside1,pch=19)
abline(h=coef(ma2.fit)[names(coef(ma2.fit))=='intercept'])

#Question 9.13

# Simulate an ARMA(1,1) process with φ= 0.7, θ= −0.5, and μ= 100. Simulate 50
# values but set aside the last 10 values to compare forecasts with actual values.
# (a)Using the first 40 values of the series, find the values for the maximum likelihood 
# estimates of φ, θ, and μ.
# (b)Using the estimated model, forecast the next ten values of the series. Plot the
# series together with the ten forecasts. Place a horizontal line at the estimate of
# the process mean.
# (c) Compare the ten forecasts with the actual values that you set aside.
# (d)Plot the forecasts together with 95% forecast limits. Do the actual values fall
# within the forecast limits?
# (e) Repeat parts (a) through (d) with a new simulated series using the same values
# of the parameters and same sample size.

#Simulate an ARMA(1,1) process with ??= 0.7, ??= ???0.5, and ??= 100. n=50 set aside 10
set.seed(12345)
arma1<-arima.sim(model = list(ar=0.7,ma=0.5),n=50)+100
arma1.aside<-window(arma1,start=41)
arma1<-window(arma1,end=40)

#part a
#fit and find estimates
arma1.fit<-arima(arma1,order = c(1,0,1),method = 'ML')
arma1.fit

#part b
#Using the estimated model, forecast the next ten values of the series
plot(arma1.fit,n.ahead=10,type='b')
abline(h=coef(arma1.fit)[names(coef(arma1.fit))=='intercept'])

#Part c
#Compare the ten forecasts with the actual values that you set aside
forecast=plot(arma1.fit,n.ahead=10,type='b')$pred
cbind(arma1.aside,forecast)

#Part d
#Plot the forecasts together with 95% forecast limits
plot(arma1.fit,n.ahead=10,type='b')
points(x=(41:50),y=arma1.aside,pch=19)
abline(h=coef(arma1.fit)[names(coef(arma1.fit))=='intercept'])

#part e
#Repeat parts (a) through (d) with a new simulated series
arma1.fit<-arima(arma1,order = c(1,0,1),method = 'ML')
arma1.fit
plot(arma1.fit,n.ahead=10,type='b')
abline(h=coef(arma1.fit)[names(coef(arma1.fit))=='intercept'])
forecast=plot(arma1.fit,n.ahead=10,type='b')$pred
cbind(arma1.aside,forecast)
plot(arma1.fit,n.ahead=10,type='b')
points(x=(41:50),y=arma1.aside,pch=19)
abline(h=coef(arma1.fit)[names(coef(arma1.fit))=='intercept'])

#Question 9.15

# Simulate an IMA(1,1) process with θ= 0.8 and θ0 = 10. Simulate 35 values, but
# set aside the last five values to compare forecasts to actual values.
# (a)Using the first 30 values of the series, find the values for the maximum likelihood estimates 
# of θand θ0.
# (b)Using the estimated model, forecast the next five values of the series. Plot the
# series together with the five forecasts. What is special about these forecasts?
# (c) Compare the five forecasts with the actual values that you set aside.
# (d)Plot the forecasts togetherwith 95% forecast limits. Do the actual values fall
# within the forecast limits?
# (e) Repeat parts (a) through (d) with a new simulated series using the same values
# of the parameters and same sample size.

#Simulate an IMA(1,1) process with ??= 0.8 and ??0= 10. n=35.set aside 5
set.seed(12345)
ima1<-arima.sim(list(order=c(0,1,1),ma=-0.8),n=35)[-1]
ima1<-ima1+10*(1:35)
ima1.aside<-window(ima1,start=31)
ima1<-window(ima1,end=30)

#part a
#Using the first 30 values of the series, find the values for mle of ??and ??0
ima1.fit<-arima(ima1,order = c(0,1,1),xreg = (1:30))
ima1.fit

#part b
#Using the estimated model, forecast the next five values of the serie
plot(ima1.fit,n.ahead=5,type='b',newxreg = (31:35))

#part c
#Compare the five forecasts with the actual values that you set aside
forecast=plot(ima1.fit,n.ahead=5,type='b',newxreg = (31:35))$pred
cbind(ima1.aside,forecast)

#part d
#Plot the forecasts together with 95% forecast limits
plot(ima1.fit,n1=22,n.ahead=5,newxreg = (31:35),pch=19)
points(x=seq(31:35),y=ima1.aside,pch=3)

#part e
#Repeat parts (a) through (d) with a new simulated series
ima1.fit<-arima(ima1,order = c(0,1,1),xreg = (1:30))
ima1.fit
plot(ima1.fit,n.ahead=5,type='b',newxreg = (31:35))
forecast=plot(ima1.fit,n.ahead=5,type='b',newxreg = (31:35))$pred
cbind(ima1.aside,forecast)
plot(ima1.fit,n1=22,n.ahead=5,newxreg = (31:35),pch=19)
points(x=seq(31:35),y=ima1.aside,pch=3)


#Question 9.23

# The time series in the data file robotgives the final position in the “x-direction”
# after an industrial robot has finished a planned set of exercises. The measurements
# are expressed as deviations from a target position. The robot is put through
# this planned set of exercises in the hope that its behavior is repeatable and thus
# predictable.
# (a)Use an IMA(1,1) model to forecast five values ahead. Obtain 95% forecast
# limits also.
# (b)Display the forecasts, forecast limits, and actual values in a graph and interpret the results.
# (c) Now use an ARMA(1,1) model to forecast five values ahead and obtain 95%
# forecast limits. Compare these results with those obtained in part (a).

data(robot)
robot

#part a
#Use an IMA(1,1) model to forecast five values ahead. Obtain 95% forecast
#limits also.
ima1.2.fit<-arima(robot,order = c(0,1,1))
ima1.2.fit
x<-plot(ima1.2.fit,n.ahead=5)
x

#part b
#Display the forecasts, forecast limits, and actual values
plot(ima1.2.fit,n.ahead=5,n1=300)

#part c
# Now use an ARMA(1,1) model to forecast five values ahead and obtain 95%
#forecast limits
arma1.2.fit<-arima(robot,order = c(1,0,1))
arma1.2.fit
y<-plot(arma1.2.fit,n.ahead=5)
y
plot(arma1.2.fit,n.ahead=5,n1=300)
abline(h=coef(arma1.2.fit)[names(coef(arma1.2.fit))=='intercept'])
