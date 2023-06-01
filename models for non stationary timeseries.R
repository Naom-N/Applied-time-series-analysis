
#TEXTBOOK: Time Series Analysis with Applications in R ~ J. Cryer & K-s. Chan
#Class projects&assignments

#Models for Nonstationary Time Series.

library(TSA)

#Question 5.11

# The data file winnebago contains monthly unit sales of recreational vehicles
# (RVs) from Winnebago, Inc., from November 1966 through February 1972.
# (a)Display and interpret the time series plot for these data.
# (b)Now take natural logarithms of the monthly sales figures and display the time
# series plot of the transformed values. Describe the effect of the logarithms on
# the behavior of the series.
# (c) Calculate the fractional relative changes, (Yt − Yt−1)/Yt−1, and compare them 
# with the differences of(natural) logarithms,∇log(Yt) = log(Yt) −log(Yt−1).


data("winnebago")

#plot the time series of these data
plot(winnebago,ylab="Monthly Unit Sales",type='l')

#plot log monthly miles
plot(log(winnebago),ylab='log(sales)',type='l')

#plot fractional changes. compare to lag 1 values
frac<-diff(winnebago)/lag(winnebago,1)
diflog<-diff(log(winnebago),ylab='log(sales)',type='l')
plot(frac,col='black',ylab='Fractional vs Diff log(sales)')
lines(diflog,col='blue')

#Question 5.13 

# The data file airpasscontains monthly U.S. air passenger miles flown from January 1949 through 
# December 1960. This is a classic time series analyzed in Box and Jenkins (1976).
# (a)Display and interpret the time series plot for these data.
# (b)Now take natural logarithms of the monthly values and display and the time
# series plot of the transformed values. Describe the effect of the logarithms on
# the behavior of the series.
# (c) Calculate the (fractional)relative changes, (Yt − Yt−1)/Yt−1, and compare
# them to the differences of (natural) logarithms,∇log(Yt). How do they compare for smaller values
# and for larger values?

data("airpass")

#plot the time series of these data
plot(airpass,ylab='Air Passenger Miles',type='l')

#plot log monthly miles
plot(log(airpass),ylab='log(miles)',type='l')

#plot fractional changes. compare to lag 1 values
frac<-diff(airpass)/lag(airpass,1)
diflog<-diff(log(airpass),ylab='log(miles)',type='l')
plot(frac,col='black',ylab='Fractional vs Diff log(miles)')
lines(diflog,col='blue')
