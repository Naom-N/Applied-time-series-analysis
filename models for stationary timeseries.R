
#TEXTBOOK: Time Series Analysis with Applications in R ~ J. Cryer & K-s. Chan
#Class projects&assignments

#Models for Stationary Time Series.

#QUESTION 4.2

# Sketch the autocorrelation functions for the following MA(2) models with parameters as specified:
# (a)θ1= 0.5 and θ2 = 0.4.
# (b)θ1= 1.2 and θ2= −0.7.
# (c) θ1= −1 and θ2= −0.6.

#Create a function
MA2acf <- function(P1,P2) {
  Y2=ARMAacf(ma=c(P1,P2),lag.max=10,pacf=FALSE)[-1]  
  # acf of MA(P1,P2) with rho[0] dropped
  plot(y=Y2, x=1:10, type='h', xlab='Lag k', ylab=expression(rho[k]), 
       axes=F, ylim=range(0,1,Y2))
  points(y=Y2, x=1:10, pch=20)
  abline(h=0)
  axis(1, at=1:10, labels= c(NA,2,NA,4,NA,6,NA,8,NA,10))
  axis(2)
  text(x=2, y=0.8, labels=expression(phi[1]==' '))
  text(x=3, y=0.8, labels=P1)
  text(x=7, y=0.8, labels=expression(phi[2]==' '))
  text(x=8, y=0.8, labels=P2)
}

#Plots with specified parameters
par(mfrow=c(2,2))
MA2acf(-0.5,-0.4)
MA2acf(-1.2,0.7)
MA2acf(1,-0.6)


#Question 4.5

# Calculate and sketch the autocorrelation functions for each of the following
# AR(1) models. Plot for sufficient lags thatthe autocorrelation function has nearly
# died out.
# (a)φ1= 0.6.
# (b)φ1= −0.6.
# (c) φ1= 0.95. (Do out to 20 lags.)
# (d)φ1= 0.3

AR1acf <- function(P) {
  Y1=ARMAacf(ar=c(P),lag.max=10,pacf=FALSE)[-1]  
  # acf of AR(P) with rho[0] dropped
  plot(y=Y1, x=1:10, type='h', xlab='Lag k', ylab=expression(rho[k]), 
       axes=F, ylim=range(0,1,Y1))
  points(y=Y1, x=1:10, pch=20)
  abline(h=0)
  axis(1, at=1:10, labels= c(1,NA,3,NA,5,NA,7,NA,9,NA))
  axis(2)
  text(x=3, y=0.8, labels=expression(paste('AR(',phi,'):  ',phi,'=')))
  text(x=5, y=0.8, labels=P)
}

par(mfrow=c(2,2))
AR1acf(0.6)	
AR1acf(-0.6)
AR1acf(0.95)
AR1acf(0.3)

#for part c- with just 20 lags
AR1acf <- function(P) {
  Y1=ARMAacf(ar=c(P),lag.max=20,pacf=FALSE)[-1]  
  # acf of AR(P) with rho[0] dropped
  plot(y=Y1, x=1:20, type='h', xlab='Lag k', ylab=expression(rho[k]), 
       axes=F, ylim=range(0,1,Y1))
  points(y=Y1, x=1:20, pch=20)
  abline(h=0)
  axis(1, at=1:20, labels= c(1,NA,3,NA,5,NA,7,NA,9,NA,11,NA,13,NA,15,NA,17,NA,19,NA))
  axis(2)
  text(x=3, y=0.8, labels=expression(paste('AR(',phi,'):  ',phi,'=')))
  text(x=5, y=0.8, labels=P)
}

#QUESTION 4.9
#similar code to ma

# Use the recursive formula of Equation (4.3.13) to calculate and then sketch the
# autocorrelation functions for the following AR(2) models with parameters as
# specified. In each case, specify whether the roots of the characteristic equation are
# real or complex. If the roots are complex, find the damping factor, R, and frequency, Θ,
#for the corresponding autocorrelation functionwhen expressed as in Equation (4.3.17), on page 73.
# (a)φ1= 0.6and φ2= 0.3.
# (b)φ1= −0.4 and φ2= 0.5.
# (c) φ1= 1.2 and φ2= −0.7.
# (d)φ1= −1 and φ2= −0.6.
# (e) φ1= 0.5and φ2= −0.9.
# (f) φ1= −0.5 and φ2= −0.6.

AR2acf <- function(P1,P2) {
  Y2=ARMAacf(ar=c(P1,P2),lag.max=10,pacf=FALSE)[-1]  
  # acf of AR(P1,P2) with rho[0] dropped
  plot(y=Y2, x=1:10, type='h', xlab='Lag k', ylab=expression(rho[k]), 
       axes=F, ylim=range(0,1,Y2))
  points(y=Y2, x=1:10, pch=20)
  abline(h=0)
  axis(1, at=1:10, labels= c(NA,2,NA,4,NA,6,NA,8,NA,10))
  axis(2)
  text(x=2, y=0.8, labels=expression(phi[1]==' '))
  text(x=3, y=0.8, labels=P1)
  text(x=7, y=0.8, labels=expression(phi[2]==' '))
  text(x=8, y=0.8, labels=P2)
}

acos(-0.3227)

par(mfrow=c(2,3))
AR2acf(0.6,0.3)
AR2acf(-0.4,0.5)
AR2acf(1.2,-0.7)
AR2acf(-1,-0.6)
AR2acf(0.5,-0.9)
AR2acf(-0.5,-0.6)



#QUESTION 4.10

#Sketch the autocorrelation functions for each of the following ARMA models:
#(a)ARMA(1,1) with φ= 0.7 and θ= 0.4.
#(b)ARMA(1,1) with φ= 0.7 and θ= −0.4.

par(mfrow=c(1,1))
set.seed(24680)
y1 <- arima.sim(n=200, model=list(ar=0.7, ma=-0.4), rand.gen=rnorm)
plot(acf(y1))
y2 <- arima.sim(n=200, model=list(ar=0.7, ma=0.4), rand.gen=rnorm)
plot(acf(y2))

