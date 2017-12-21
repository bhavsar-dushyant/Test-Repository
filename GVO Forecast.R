## Useful libraries for time series forecast. There are overlaps in functionalities ##
library(smooth)
library(forecast)
library(graphics)
library(datasets)
library(tseries)
library(fpp2)
#install.packages("xlsx")
library(xlsx)
# Import datasets #

# Load CompanyX data from Library{datasets}#

CompanyX <- ts(GVO_Puma[[2]], start=c(2013, 1), end=c(2017, 12), frequency=12)
plot(CompanyX)
BY <- CompanyX


BY3 <- ma(BY, order=3) 
BY9 <- ma(BY, order=5)
BY19 <- ma(BY, order=7)

ts.plot(BY, BY3, BY9, BY19, lty=c(1:4), col=c('black','red','dark blue', 'forest green'))
BY3
# Forecasting using MA #
#Library{smooth} #

BY3H <- sma(BY, order=3, h=17, holdout=T, intervals="p", level=0.95) 
BY9H <- sma(BY, order=9, h=10, holdout=T, intervals="p", level=0.95) 
plot(BY3H)
BY3H$forecast
BY9H$forecast
#################################
## TS Decomposition ##

monthplot(CompanyX, col='maroon', main="Seasonal Subplot")


# Decompose time series into multiplicative components #
CompX.mult <- decompose(CompanyX, type="multiplicative")
plot(CompX.mult$figure, type="l") # Plots seasonality indices
plot(CompX.mult$trend, type="l") # Plots trend
plot(CompX.mult)
CompX.mult$trend
CompX.mult$random
# Predictive Accuracy: MAPE #

## CompanyX  ##
CompX1 <- window(CompanyX, start=c(2013,1), end=c(2017,1))
CompXHO <- window(CompanyX, start=c(2017,2), end=c(2017,12))

# Predict next 17 future values and compare #


## CompanyX ##

CompX.hw1 <- hw(CompanyX, seasonal='m', initial='o')
CompX.hw1$model
# simple exponential - models level
fit1 <- HoltWinters(CompanyX, alpha=0.0527, beta=FALSE, gamma=FALSE, seasonal = "mult")
plot(fit1)

# double exponential - models level and trend
fit2 <- HoltWinters(CompanyX, alpha=0.0527, beta=0.0001, gamma=FALSE)
plot(fit2)

# triple exponential - models level, trend, and seasonal components
fit3 <- HoltWinters(CompanyX, alpha=0.3879, beta=0.0006, gamma=0.0001, seasonal = "mult")
plot(fit3, main="HW (alpha=0.3, beta=0.01, gamma=0.6)")

# Final tunned Holt-Winter model
#CompXFC <- hw(CompX1, seasonal='m',alpha=0.0628, beta=0.0001, gamma=0.0001, h=17 )

CompXFC <- hw(CompX1, seasonal='m',alpha=0.3879, beta=0.0001, gamma=0.0001, h=18 )

#CompXFC <- hw(CompX1, seasonal='m',alpha=0.03, beta=0.01, gamma=0.6, h=18 )
plot(CompXFC)
CompXFC$mean



CompXFC$fitted
write.xlsx(CompXFC$mean, "/ALBERTA2.xlsx")

## Forecast by Exponential Smoothing ##

Vec2<- cbind(CompXHO,CompXFC$mean)
ts.plot(Vec2, col=c("blue", "red"), main="CompanyX: Actual vs Forecast")
MAPE <- mean(abs(Vec2[,1]-Vec2[,2])/Vec2[,1])
MAPE

# Detrend Data #

BY1 <- lag(BY, k=-1)
df1 <- diff(BY, order=1)
cbind(BY, BY1, diff(BY, order=1))

####################
# Test for Stationarity
library(tseries)
adf.test(na.omit(CompX.mult$random))

######################

# Autocorrelation #

######################

BY1 <- lag(BY, k=-1)
df1 <- diff(BY, order=1)
cbind(BY, BY1, diff(BY, order=1))

AC1 <- acf(BY, lag.max=25) # Autocorrelation 
AC1
PAC1 <- pacf(BY, lag.max=25) # Partial autocorrelation
PAC1
####################
# Test for Stationarity
adf.test(na.omit(Champ.add$random)) # library(tseries)
adf.test(na.omit(AirP$random))

######################
# Differencing
Clay <- BY
adf.test(Clay)

Clay1 <- lag(Clay, k=-1)
Claydf1<- diff(Clay) # Lag 1 difference

Clay2 <- lag(Clay, k=-2)
Claydf2<- diff(Clay, lag=2) # Lag 2 difference

plot(Claydf1, col="purple", main="First Difference: Clay Brick")
adf.test(Claydf1)

acf(Clay, main="Clay Brick")
acf(Claydf1, main="First Difference: Clay Brick")

########## BASF ##########
plot(BASF, col="dark green")
acf(BASF, lag=30, main="BASF Monthly Average")
BASF1 <- diff(BASF, order=1)
acf(BASF1, lag=30, main="BASF First Difference")
pacf(BASF, lag=30, main="BASF Monthly Average")
pacf(BASF, lag=30, main="BASF First Difference")

BASF <- BY
BASF.autofit <- auto.arima(BASF)
BASF.arima.fit <- arima(BASF, c(1, 1, 0))
Box.test(BASF.arima.fit$residuals, lag = 60, type = "Ljung-Box")
Box.test(BASF.autofit$residuals, lag = 24, type = "Ljung-Box")

##### BMW ######
plot(BMW, col="blue")
acf(BMW, lag=100)
pacf(BMW, lag=100)

BMW1 <- diff(BMW, order=1)
plot(BMW1, ylab="First Difference: BMW", col="brown")
acf(BMW1, lag=50)
pacf(BMW1, lag=50)

BMW.autofit <- auto.arima(BMW)

BMW.arima.fit <- arima(BMW, c(1, 1, 0))
Box.test(BMW.arima.fit$residuals, lag = 36, type = "Ljung-Box")
Box.test(BMW.autofit$residuals, lag = 36, type = "Ljung-Box")
adf.test(BMW)
adf.test(BMW1)


AirP1.mult <- decompose(AirPax1, type="m")
lapply(AirP1.mult, as.numeric)
fcst.AirPax <- forecast(AirP1.mult)


