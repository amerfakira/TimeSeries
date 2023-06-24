# ARIMA and S-ARIMA Models of General Motors stock prices spanning from January 
# 3rd 2011, to December 27, 2019 forecasting.


# Before beginning, it is important to note that 
# ARIMA models are shown to be better than machine 
# learning models because they do not over fit the data.
# Over-fitting makes us prone to incorrectly believing
# our models are making highly accurate predictions,
# generally because we have added to many variables.
# Exponential Smoothing is another type of model that 
# competes with ARIMA models, but in general ARIMA models
# are more accurate.



#LOAD
# The first step is to load our data of General Motors daily closing stock prices
# into a variable called 'GM'.

GM <- readRDS("Downloads/GM.rds")


# To have an initial look at the data we autoplot it.
autoplot(GM)

# 2

install.packages("plotly")
install.packages("ggplot2")
install.packages("fpp2")
library(fpp2)
library(fUnitRoots)
library(forecast)
# Checking for trends
autoplot(GM)
adfTest(GM, type = 'c')

# If p - value in the adf test is larger than 5%, we fail to reject the null hypothesis, and there may be unit root in the time series


# H0: The series has unit root
# HA: The series does not have unit root

# Our p - value is 0.4635, therefore we fail to reject the null hypothesis. This 
# means that the series may have unit root.


#3

dGM <- diff(GM)
autoplot(dGM)

# This plot looks like white noise.

#4

adfTest(dGM, type = 'nc')

# Our p- value is less than .01. Therefore, we reject the null hypothesis that the series has unit root. 

# d in ARIMA(p,d,q) should be 1. This is because our first difference has made the series stationary.

# 5 
Acf(dGM, lag = 24)
Pacf(dGM, lag = 24)

# because none of the first few bars are outside the 95% confidence interval, then this is another strong indicator that the series is now white noise.

# 6

# In general, for white noise, p is 0 and q is also 0. From all indicators, it seems we are looking at white noise. Therefore, right now we are expecting our ARIMA(p,d,q) to be ARIMA(0,1,0)
# dYt is now white noise, but Yt is random walk

# 7
auto.arima(GM)

# 8
auto.arima(GM, approximation=FALSE, stepwise=FALSE )


# Forecasting using ARIMA/S-ARIMA Models

# 1)
Tr <- read.csv('Downloads/Tractor-Sales.csv')

# Converting into a time series
Trs <- ts(Tr[,-1], start = c(2003,1), frequency = 12)

# 2)
autoplot(Trs)

# 3)
model <- auto.arima(Trs)
summary(model)

# ARIMA(p,d,q)(P,D,Q)m 
# (p,d,q) is our non - seasonal part and is our first ARIMA model
# (P,D,Q) is our seasonal part, and has a separate ARIMA model
# m tells us the number of periods within one cycle

# The ARIMA model is ARIMA(2,1,1)(0,1,0)[12]

# Question 4
autoplot(Trs) + autolayer(model$fitted)

# Question 5 
forecasted <- forecast(model, h = 24)

# Question 6
autoplot(forecasted)

# ARIMA models are shown to be better than machine learning models because they do not over fit the data
# Exponential Smoothing is another type of model

#If you forecast using Decomposition Method you are using exponential smoothing models
# ARIMA models are generally better than exponential smoothing models, but they can be better sometimes




