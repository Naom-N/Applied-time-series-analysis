
#TEXTBOOK: Time Series Analysis with Applications in R ~ J. Cryer & K-s. Chan
#Class projects&assignments

#Model Specification

#question 6.20

# Simulate an AR(1) time series with n= 48 and with φ= 0.7.
# (a)Calculate the theoretical autocorrelations at lag 1 and lag 5 for this model.
# (b)Calculate the sample autocorrelations at lag 1 and lag 5 and compare the values with 
# their theoretical values. Use Equations (6.1.5) and (6.1.6) page 111,to quantify the comparisons. 
# (c) Repeat part (b) with a new simulation. Describe how the precision of the estimate varies 
# with different samples selected under identical conditions.
# (d)If software permits, repeat the simulation of the series and calculation of r1 and r5
# many times and form the sampling distributions of r1 and r5
# . Describe
# how the precision of the estimate varieswith different samples selected under
# identical conditions. How well does the large-sample variance given in Equation (6.1.5) 
# on page 111, approximate the variance in your sampling distribution?
  
  
#Simulate an AR(1) time series with n= 48 and with ??= 0.7
set.seed(92397)
sim.1.AR1<-arima.sim(model=list(ar=c(0.7)),n=48)
plot(sim.1.AR1)
acf(sim.1.AR1,lag.max=5)[1:5]


#part c
sim.1.AR1<-arima.sim(model=list(ar=c(0.7)),n=48)
acf(sim.1.AR1,lag.max=5)[1:5]

#part d
set.seed(92397) 
r1=rep(NA,10000)
r5=r1 # We are doing 10,000 replications.
for (k in 1:10000) {
  sim.1.AR1=arima.sim(model=list(ar=0.7),n=48);
  r1[k]=acf(sim.1.AR1,xaxp=c(0,10,5),plot=F)$acf[1];
  r5[k]=acf(sim.1.AR1,xaxp=c(0,10,5),plot=F)$acf[5]
  }
hist(r1)
mean(r1)
sd(r1)
median(r1)
hist(r5)
mean(r5)
sd(r5)
median(r5)

par(mfrow=c(1,2))

#question 6.21

# Simulate an MA(1) time series with n= 60 and with θ= 0.5.
# (a)Calculate the theoretical autocorrelation at lag 1 for this model.
# (b)Calculate the sample autocorrelation at lag 1, and compare the value with its
# theoretical value. Use Exhibit 6.2 on page 112, to quantify the comparisons. 
# (c) Repeat part (b) with a new simulation. Describe how the precision of the estimate varies 
# with different samples selected under identical conditions.
# (d)If software permits, repeat the simulation of the series and calculation of r1
# many times and form the sampling distribution of r1
# . Describe how the precision of the estimate varies with different samples selected under 
# identical conditions. How well does the large-sample variance given in Exhibit 6.2 on page
# 112, approximate the variance in your sampling distribution? 
  
#Simulate an MA(1) time series with n= 60 and with ??= 0.5
set.seed(92397)
sim.1.MA1<-arima.sim(model=list(ma=c(-0.5)),n=60)
plot(sim.1.MA1)
acf(sim.1.MA1,lag.max=5)[1]

#part c
sim.1.AR1<-arima.sim(model=list(ma=c(-0.5)),n=60)
acf(sim.1.MA1,lag.max=5)[1]

#part d
set.seed(92397) 
r1=rep(NA,10000)
r5=r1 # We are doing 10,000 replications.
for (k in 1:10000) {
  sim.1.MA1=arima.sim(model=list(ar=-0.5),n=60);
  r1[k]=acf(sim.1.MA1,xaxp=c(0,10,5),plot=F)$acf[1]
  }
hist(r1)
mean(r1)
sd(r1)
median(r1)


#question 6.23

# Simulate an AR(1) time series with φ= 0.6, with
# (a)n= 24, and estimate ρ1= φ= 0.6 with r1;
# (b)n= 60, and estimate ρ1= φ= 0.6 with r1;
# (c) n= 120, and estimate ρ1= φ= 0.6 with r1.
# (d)For each of the series in parts (a), (b), and (c), compare the estimated values
# with the theoretical value. Use Equation (6.1.5) on page 111, to quantify the
# comparisons. In general, describe how the precision of the estimate varies
# with the sample size

#AR(1) time series with ??= 0.6
set.seed(92397)
sim.2.AR1<-arima.sim(model=list(ar=c(0.6)),n=24)
acf(sim.2.AR1,lag.max=5)[1]
sim.3.AR1<-arima.sim(model=list(ar=c(0.6)),n=60)
acf(sim.3.AR1,lag.max=5)[1]
sim.3.AR1<-arima.sim(model=list(ar=c(0.6)),n=120)
acf(sim.3.AR1,lag.max=5)[1]


#question 6.24

# Simulate an MA(1) time series with θ= 0.7, with
# (a)n= 24, and estimate ρ1with r1;
# (b)n= 60, and estimate ρ1with r1;
# (c) n= 120, and estimate ρ1with r1.
# (d)For each of the series in parts (a), (b), and (c), compare the estimated values of ρ1
# with the theoretical value. Use Exhibit 6.2 on page 112, to quantify the
# comparisons. In general, describe how the precision of the estimate varies
# with the sample size.

#MA(1) time series with ??= 0.7
set.seed(92397)
sim.2.MA1<-arima.sim(model=list(ma=(-0.7)),n=24)
acf(sim.2.MA1,plot = FALSE)$acf[1]
sim.3.MA1<-arima.sim(model=list(ma=c(-0.7)),n=60)
acf(sim.3.MA1,plot = FALSE)$acf[1]
sim.3.MA1<-arima.sim(model=list(ma=c(-0.7)),n=120)
acf(sim.3.MA1,plot = FALSE)$acf[1]


#question 6.25

# Simulate an AR(1) time series of length n= 36 with φ= 0.7.
# (a)Calculate and plot the theoretical autocorrelation function for this model. Plot
# sufficient lags until the correlations are negligible.
# (b)Calculate and plot the sample ACF for your simulated series. How well do the
# values and patterns match the theoretical ACF from part (a)?
# (c) What are the theoretical partial autocorrelations for this model?
# (d)Calculate and plot the sample ACF for your simulated series. How well do the
# values and patterns match the theoretical ACF from part (a)? Use the
# large-sample standard errors reported in Exhibit 6.1 on page 111, to quantify
# your answer.
# (e) Calculate and plot the sample PACF for your simulated series. How well do
# the values and patterns match the theoretical ACF from part (c)? Use the
# large-sample standard errors reported on page 115 to quantify your answer

#part a
acf1<-ARMAacf(ar=0.7,lag.max = 15)
plot(acf1[-1],type = "h",ylab = 'ACF')
abline(h=0)
round(acf1,digits = 3)

#part b
set.seed(92397)
sim.5.AR1<-arima.sim(model=list(ar=c(0.7)),n=36)
round(acf(sim.5.AR1,plot = FALSE)$acf,digits = 3)
acf(sim.5.AR1,lag.max = 15)

pacf(acf1)
pacf(sim.5.AR1)

#Question 6.26

# Simulate an MA(1) time series of length n= 48 with θ= 0.5.
# (a)What are the theoretical autocorrelations for this model?
# (b)Calculate and plot the sample ACF for your simulated series. How well do the
# values and patterns match the theoretical ACF from part (a)?
# (c) Calculate and plot the theoretical partial autocorrelation function for this
# model. Plot sufficient lags until the correlations are negligible. (Hint: See
# Equation (6.2.6) on page 114.)
# (d)Calculate and plot the sample PACF for your simulated series. How well do
# the values and patterns match the theoretical ACF from part (c)?

set.seed(92397)
sim.5.MA1<-arima.sim(model=list(ma=(-0.5)),n=48)

#part a. Theoretical autocorrelations

acf(sim.5.MA1)
theta<-0.5
phikk<-rep(NA,10)
for (k in 1:10) {
  phikk[k]<--(theta^k*(1-theta^2))/(1-theta^(2*(k+1)))
}

plot(phikk,type = 'h')
abline(h=0)


pacf(sim.5.MA1)


#question 6.29

# Simulate a mixed ARMA(1,1) model of length n= 60 with φ= 0.4 and θ= 0.6.
# (a)Calculate and plot the theoretical autocorrelation function for this model. Plot
# sufficient lags until the correlations are negligible.
# (b)Calculate and plot the sample ACF for your simulated series. How well do the
# values and patterns match the theoretical ACF from part (a)?
# (c) Calculate and interpret the sample EACFfor this series. Does the EACF help
# you specify the correct orders for the model?
# (d)Repeat parts (b) and (c) with a new simulation using the same parameter values and sample size.
# (e) Repeat parts (b) and (c) with a new simulation using the same parameter values but sample 
# size n= 36.
# (f) Repeat parts (b) and (c) with a new simulation using the same parameter values but sample 
# size n= 120

#Simulate a mixed ARMA(1,1) model of length n= 60 with ??= 0.4 and ??= 0.6
arma1<-ARMAacf(ar=0.4,ma=-0.6,lag.max = 15)
plot(arma1,type='h')
abline(h=0)

#Part b
set.seed(92397)
simARMA<-arima.sim(model = list(ar=0.4,ma=-0.6),n=60)
acf(simARMA,xaxp=c(0,20,10))

#part c
eacf(simARMA)

#part d
simARMA<-arima.sim(model = list(ar=0.4,ma=-0.6),n=60)
acf(simARMA,xaxp=c(0,20,10))

eacf(simARMA)

#part e
simARMA<-arima.sim(model = list(ar=0.4,ma=-0.6),n=36)
acf(simARMA,xaxp=c(0,20,10))

eacf(simARMA)

#part f
simARMA<-arima.sim(model = list(ar=0.4,ma=-0.6),n=120)
acf(simARMA,xaxp=c(0,20,10))

eacf(simARMA)
