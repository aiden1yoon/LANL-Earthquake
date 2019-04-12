########Final Project########

# This file export runs a chi square test to check for normality of acoustic
# signal distribution 

#Set Working Directory 
setwd("C:/Users/h2osk/OneDrive/Documents/189")
#install.packages("data.table")
#install.packages("ggplot2")
#install.packages("matrixStats")
#install.packages("gridExtra")
#install.packages("forecast")
#install.packages("tseries")

#Load dependencies
library(data.table)
library(ggplot2)
library(matrixStats)
library(gridExtra)
library(forecast)
library(tseries)

# Load  first dataset 
df1<-fread("train_X_1.csv")
df2<- df1[seq(1,length(df1$acoustic_data),100)]
# Plot histogram 
ggplot(df1, aes(x=acoustic_data)) + geom_histogram(binwidth=1) +xlim(-20,20)


######################
#Tests for Normality##
######################

# Chi square test
#Put into proportion table first 

#Create vector of culmulative proababilities for quantile
probability<-c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

#Obtain quantiles 
a<-quantile(df1$acoustic_data, probability)
b<-as.numeric(a[1:length(a)])

#Create proportion table
proportion_table<-as.table(as.numeric(table(cut(df1$acoustic_data, breaks=c(-Inf,0,2,3,4,5,6,7,9, Inf)))))

#Perform Chi Square Goodness of Fit
chisq.test(proportion_table, p=c(0.1, 0.1, 0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.1))

#The data does not follow a normal distribtion 



# K-S Test 
#Generate Normal distributed numbers
normal_generates<-rnorm(nrow(df1), mean(df1$acoustic_data), sd(df1$acoustic_data))

#Perform KS test 
ks.test(df1$acoustic_data, normal_generates)
#P Value is low, so not normal




# Shapiro Wilks test for normality
#Pull 5000 samples to allow the test to work 
ad_sample<-sample(df1$acoustic_data, replace=F, 5000)
shapiro.test(ad_sample)
# Not normal becuase p-values are very small 



###############################################
############### Time Series ###################
###############################################
waves_ts= ts(df2)
  
ts.plot(waves_ts, xlab= "", ylab="acoustic data")
adf.test(waves_ts)


adf.test(waves_ts, alternative = "stationary")
auto.arima(waves_ts, seasonal = FALSE)

fit= auto.arima(waves_ts,seasonal = FALSE)
tsdisplay(residuals(fit), lag.max = 50,main = '(5,1,0) Model Residuals')
fit2 =arima(waves_ts, order= c(5,1,8))

tsdisplay(residuals(fit2), lag.max=50, main = '(5,1,8) Model Residuals')

arima(x=waves_ts, order = c(5,1,8))

fcast <- forecast(fit2, h=100000)
plot(fcast)

