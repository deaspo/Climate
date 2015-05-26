##Time Series Analysis of Kisumu Rainfall Data
library('plyr')
#set working directory
setwd("C:/Users/kidi wa logha/Dropbox/enviromental statistics/kisumu")
#load the data
kisumu=read.table('kisumu.csv',header=T,sep=',')
#inhouse cleaning
kisumu$Season=NULL;kisumu$X=NULL;kisumu$Type=NULL;kisumu$Year=NULL
################################stack the data
kis_tack=stack(kisumu)
names(kis_tack)=c('Rainfall','Month')
##TIme series analysis
attach(kis_tack)
#smoothing the data
rain=ts(Rainfall,start=c(1961,1),frequency=12)
#rain=matrix(rain,50,12)
kis_decomp=decompose(sqrt(rain))
############################
plot(kis_decomp)
########
trend=kis_decomp$trend
seasonal=kis_decomp$seasonal
ts.plot(cbind(trend[7:594],trend[7:594]*seasonal[7:594]),lty=1:2)
#from the above two plots, I have identified trend and seasonal effects
#and i have to deseasonalize the data and remov the trend
#Detection of the autocorrelation
plot(ts(kis_decomp$random[7:594]))
acf(kis_decomp$random[7:594])
#standard deviation
sd(rain[7:594])#77.89395
#########################
sd(rain[7:594])-kis_decomp$trend[7:594]#standard deviation of the series after
#subtracting the the trend estimate
sd(kis_decomp$random[7:594])#standard deviaton after seasonal adjustment#2.811256
#reduction in standard deviation shows that the seasonal adjustment has been very
#effective
############################################################
#ARIMA
arima(sqrt(rain)[7:594],order=c(1,0,0),seasonal=list(order=c(1,0,0),period=12))
####order 2
arima(rain[7:594],order=c(2,0,0),seasonal=list(order=c(1,1,0),period=4))
##log transformation
arima(log(jj),order=c(1,0,0),seasonal=list(order=c(1,1,0),period=4))
arima(log(jj),order=c(2,0,0),seasonal=list(order=c(1,1,0),period=4))







