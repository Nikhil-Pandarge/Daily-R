kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)
kings
kingstimeseries <- ts(kings)
kingstimeseries
help(ts)
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
births
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))
birthstimeseries
plot.ts(kingstimeseries)
library(TTR)
installed.packages("TTR")
