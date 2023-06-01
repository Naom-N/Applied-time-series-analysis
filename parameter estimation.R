
#TEXTBOOK: Time Series Analysis with Applications in R ~ J. Cryer & K-s. Chan
#Class projects&assignments

#Parameter Estimation

#Question 7.9 

# Simulate an MA(1) series with θ= 0.8 and n= 48.
# (a)Find the method-of-moments estimate of θ.
# (b)Find the conditional least squares estimate of θand compare it with part (a).
# (c) Find the maximum likelihood estimate of θand compare it with parts (a) and (b).
# (d)Repeat parts (a), (b), and (c) with a new simulated series using the same
# parameters and same sample size. Compare your results with your results
# from the first simulation

set.seed(12345)
ma1.1<-arima.sim(model = list(ma=-0.8),n=48)

# Below is a function that computes the method of moments estimator of
# the MA(1) coefficient of an MA(1) model
estimate.ma1.mom=function(x){r=acf(x,plot=F)$acf[1]; if (abs(r)<0.5) 
  return((-1+sqrt(1-4*r^2))/(2*r)) else return(NA)}

#Find the method-of-moments estimate of ??.
estimate.ma1.mom(ma1.1)

#Find the conditional least squares estimate of ??and compare it with part (a)
arima(ma1.1,order = c(0,0,1),method = 'CSS')

#Find the maximum likelihood estimate of ??and compare it with parts (a) and
#(b).
arima(ma1.1,order = c(0,0,1),method = 'ML')

#part d. repeat a,b and c
ma1.1<-arima.sim(model = list(ma=-0.8),n=48)
estimate.ma1.mom(ma1.1)
arima(ma1.1,order = c(0,0,1),method = 'CSS')
arima(ma1.1,order = c(0,0,1),method = 'ML')

#Question 7.14

# Simulate an AR(1) series with φ= −0.5 and n= 60.
# (a)Find the method-of-moments estimate of φ.
# (b)Find the conditional least squares estimate of φand compare it with part (a).
# (c) Find the maximum likelihood estimate of φand compare it with parts (a) and (b).
# (d)Repeat parts (a), (b), and (c) with a new simulated series using the same
# parameters and same sample size. Compare your results with your results
# from the first simulation

set.seed(12345)
ar1.1<-arima.sim(model = list(ar=-0.5),n=60)
acf(ar1.1,lag.max = 1)[1]

#Find the conditional least squares estimate of  ?? and compare it with part (a)
arima(ar1.1,order = c(0,0,1),method = 'CSS')

#Find the maximum likelihood estimate of ?? and compare it with parts (a) and
#(b).
arima(ar1.1,order = c(0,0,1),method = 'ML')

#part d
ar1.1<-arima.sim(model = list(ar=-0.5),n=60)
acf(ar1.1,lag.max = 1)[1]
arima(ar1.1,order = c(0,0,1),method = 'CSS')
arima(ar1.1,order = c(0,0,1),method = 'ML')

#Question 7.16

# Simulate an AR(2) series with φ1= 0.6, φ2= 0.3, and n= 60.
# (a)Find the method-of-moments estimates of φ1and φ2.
# (b)Find the conditional least squares estimates of φ1 and φ2and compare them with part (a).
# (c) Find the maximum likelihood estimates of φ1 and φ2 and compare them with parts (a) and (b).
# (d)Repeat parts (a), (b), and (c) with a new simulated series using the same
# parameters and same sample size. Compare these results to your results from
# the first simulation.

set.seed(12345)
ar2.1<-arima.sim(model = list(ar=c(0.6,0.3)),n=60)

#Find the method-of-moments estimates of ??1 and ??2
ar(ar2.1,order.max=2,AIC=F,method='yw') # method of moments

#Find the conditional least squares estimates of ??1 and ??2and compare them
#with part (a).
ar(ar2.1,order.max=2,AIC=F,method='ols') # conditional sum of squares

# Find the maximum likelihood estimates of ??1 and ??2 and compare them with
#parts (a) and (b)
ar(ar2.1,order.max=2,AIC=F,method='mle') # maximum likelihood

#part d
ar2.1<-arima.sim(model = list(ar=c(0.6,0.3)),n=60)
ar(ar2.1,order.max=2,AIC=F,method='yw') # method of moments
ar(ar2.1,order.max=2,AIC=F,method='ols') # conditional sum of squares
ar(ar2.1,order.max=2,AIC=F,method='mle') # maximum likelihood

#Question 7.26

# Consider the AR(1) model specified for the color property time series displayed
# in Exhibit 1.3 on page 3. The data are in the file named color. 
# (a)Find the method-of-moments estimate of φ.
# (b)Find the maximum likelihood estimate of φand compare it with part (a)

data(color)

#Find the method-of-moments estimate of ??
ar(color,order.max=1,AIC=F,method='yw') # method of moments

ar(color,order.max=1,AIC=F,method='ols') # conditional sum of squares

#Find the maximum likelihood estimate of ??and compare it with part (a)
ar(color,order.max=1,AIC=F,method='mle') # maximum likelihood

#Question 7.27

# Exhibit 6.31 on page 139 suggested specifying either an AR(1) or possibly an
# AR(4) model for the difference of the logarithms of the oil price series. The data
# are in the file named oil.price.
# (a)Estimate both of these models using maximum likelihood and compare it with
# the results using the AIC criteria.
# (b)Exhibit 6.32 on page 140 suggested specifying an MA(1) model for the difference of the logs. Estimate this model by maximum likelihood and compare to
# your results in part (a)

data("oil.price")
arima(diff(log(oil.price)),order=c(1,0,0),method='ML') # maximum likelihood
arima(diff(log(oil.price)),order=c(4,0,0),method='ML')

#part b
arima(diff(log(oil.price)),order=c(0,0,1),method='ML')

#Question 7.28
data("deere3")

#Estimate the parameters of an AR(1) model for this series.
arima(deere3,order=c(1,0,0))

#Estimate the parameters of an AR(2) model for this series.
arima(deere3,order=c(2,0,0))

#Question 7.29
data("robot")


#Estimate the parameters of an AR(1) model for these data
arima(robot,order=c(1,0,0))

#Estimate the parameters of an IMA(1,1) model for these data.
arima(robot,order=c(0,1,1))

# Compare the results from parts (a) and (b) in terms of AIC.