# install.packages("Hmisc")
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("lubridate")
# install.packages("pastecs")
# install.packages("outliers")
# install.packages("corrplot") 
# install.packages("rcompanion") 
# install.packages("nortest")
# install.packages("ggplot2")
# install.packages("sqldf")
# install.packages("tseries")
# install.packages("forecast")
# install.packages("xlsx")

library(Hmisc)
library(dplyr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(forcats)
library(scales)
library(tidyr)
library(pastecs)
library(outliers)
library(corrplot)
library(rcompanion)
library(nortest)
library(ggplot2)
library(sqldf)
library(tseries)
library(forecast)
library(xlsx)

uk <- read.xlsx("C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Time Series Forecast -  UK Air Travel\\UK Outward Passengers Movement.xls", sheetIndex=1)
head(uk,5)

NROW(uk)
NCOL(uk)

str(uk)

summary(uk)

describe(uk)

# User-Defined function to find NAs for uk dataframe

findMissingValues <- function(x) {
na_cnt <- colSums(is.na(x))
columns <- colnames(x)   
na_percent <- apply(x, 2, function(col)sum(is.na(col))*100/length(col))
tot_cnt <- apply(x, 2, function(col)length(col))
res <- data.frame(columns,tot_cnt,na_cnt,na_percent)
print(res[order(res$na_percent),])           
gg <- ggplot(data = res, mapping = aes(x = na_percent, y = columns)) + geom_point(alpha = 1, aes(color = columns))
gg              
                 
}

findMissingValues(uk)            

# Drop last 2 rows since they dont have any data 

uk <- uk[!uk$Year==2006,] 


# Create a function that will find and cap the outliers.
# An outlier is considered if it is below the (Q1 – 1.5*IQR) or above (Q3 + 1.5*IQR)
# Or, an outlier is considered if it is below the 5th percentile or above 95th percentile
# We will cap the low and high outliers with 5th and 95th percentile values respectively

HandleOutlier <- function(x,names){
  
   m1 <- mean(x, na.rm = T) 
   cat("\n Mean with outliers:\n", round(m1, 4))
   OutVals = boxplot(x, main="Boxplot with outliers",horizontal = TRUE,col = "red")$out
   cat("\n Pre-treatment Outliers:\n", OutVals)
   
   which(x %in% OutVals)    
#   hist(x, main="Histogram with outliers", xlab=NA, ylab=NA, col="red",)
#   plot(x,xlab="Plot with Outliers", ylab="values", pch="*", col="red", cex=2)

# Method 1: Capping with 5th and 95th percentiles for outliers defined by IQR 
    
#   qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
#   caps <- quantile(x, probs=c(.05, .95), na.rm = T)
#   H <- 1.5 * IQR(x, na.rm = T)
#   x[x < (qnt[1] - H)] <- caps[1]
#   x[x > (qnt[2] + H)] <- caps[2]
    
# Method 2: Capping outliers with 5th and 95th percentiles 
    quantiles <- quantile(x, c(.05, .95), na.rm = T)
    x[ x < quantiles[1] ] <- quantiles[1]
    x[ x > quantiles[2] ] <- quantiles[2]
    
# Method 3: We can replace the outliers with NAs and later impute NAs using MICE or kNN  
    
#  y[x < (qnt[1] - H)] <- NA
#  y[x > (qnt[2] + H)] <- NA
    
# Method 4: Imputation with mean / median / mode
    
#  x[x < (qnt[1] - H)] <- median(x, na.rm = T)
#  x[x > (qnt[2] + H)] <- median(x, na.rm = T)
#  x[x < (qnt[1] - H)] <- mean(x, na.rm = T)
#  x[x > (qnt[2] + H)] <- mean(x, na.rm = T)
#  x[x < (qnt[1] - H)] <- mode(x, na.rm = T)
#  x[x > (qnt[2] + H)] <- mode(x, na.rm = T)
    
   m2 <- mean(x, na.rm = T)
   cat("\n Mean without outliers:\n", round(m2, 4))
   OutVals = boxplot(x, main="Boxplot without outliers",horizontal = TRUE,col = "green")$out
      
   cat("\n Post-treatment Outliers:\n", OutVals)
    
   which(x %in% OutVals)
#   hist(x, main="Histogram without outliers", xlab=NA, ylab=NA, col="green")
#   plot(x,xlab="Plot without Outliers", ylab="values", pch="*", col="green", cex=2)

    return(x)
}

# Check for any outliers
uk$Ireland <- HandleOutlier(uk$Ireland)

HandleOutlier(uk$Other.EU.not.Ireland.)

uk$Rest.of.Europe.and.Med <- HandleOutlier(uk$Rest.of.Europe.and.Med)

uk$Rest.of..World <- HandleOutlier(uk$Rest.of..World)

HandleOutlier(uk$Total)

# Couple of columns had outliers  : uk$Other.EU.not.Ireland. and uk$Rest.of..World. These were capped with 5th and 95th percentiles.  
# Total column has no outliers

uk1 <- dplyr::select(uk,Year,Quarter,Total)
head(uk1,5)

# uk$Total is the univariate data which we are converting to time series. start gives the starting time of the data, in this case, its Jan 2011. As it is a quarterly data so ‘frequency=4’.

uk2 <- ts(uk1$Total, start = c(1996,1), frequency = 4)
uk2
# This 'Error in repr_matrix_generic' error only appears in Jupyter. It does nt appear in RStudio

# windows()
plot(uk2, main="Time Series of Total Outward Pasengers",
    type="lines", xlab="Time", ylab="# of Passengers on All flights",
    col="blue", lty=1, lwd=2)

ggplot2::autoplot(uk2) + labs(x ="Date", y = "# of Passengers on All flights", title="Time Series of Total Outward Pasengers") 

boxplot(uk2~cycle(uk2),xlab="Quarter", ylab = "Passenger Numbers" ,main ="Quarterly Air Passengers Boxplot from 1996 to 2005")

#Here we get 4 components:

# Observed – the actual data plot
# Trend – the overall upward or downward movement of the data points
# Seasonal – any monthly/yearly pattern of the data points
# Residual – unexplainable part of the data

decomposeUKa <- decompose(uk2,"additive")
decomposeUKa
autoplot(decomposeUKa)

decomposeUKm <- decompose(uk2,"multiplicative")
autoplot(decomposeUKm)

# Extracting the components individually

autoplot(decomposeUKa$seasonal)
autoplot(decomposeUKa$trend)
autoplot(decomposeUKa$random)

ukma <- ma(uk2, order=12) 
ts.plot(uk2, ukma, lty=c(1:2), col=c('black','red'))

rep(decomposeUKa$figure, 6)
plot(decomposeUKa$figure, type="l") 

plot(decomposeUKa$trend, type="l") # Plots trend

plot(decomposeUKa$random, type="l")

recomposeUKa = decomposeUKa$seasonal + decomposeUKa$trend + decomposeUKa$random
plot(as.ts(recomposeUKa))

ggplot2::autoplot(as.ts(recomposeUKa)) + labs(x ="Date", y = "# of Passengers on All flights", title="Time Series of Total Outward Pasengers")

# Plot monthly and seasonal components

monthplot(uk2, main="Quarterly Variation of Total Passengers")
seasonplot(uk2, main="Seasonal Variation of Total Passengers") 


adf.test(uk2) 

kpss.test(uk2) 

par(mfrow=c(1,2)) 

acf(uk2,plot=TRUE)
pacf(uk2,plot=TRUE)



par(mfrow=c(1,2)) 

autoplot(acf(uk2,plot=FALSE))+ labs(title="Correlogram of Air Passengers from 1996 to 2006") 
autoplot(pacf(uk2,plot=FALSE))+ labs(title="Correlogram of Air Passengers from 1996 to 2006") 


tsDiff <- diff(uk2,lag=1)

ggplot2::autoplot(tsDiff) + labs(x ="Date", y = "Differenced Total", title="Plot the first difference time series(1996-2006)") 

adf.test(tsDiff) 

kpss.test(tsDiff)

tsLog <- log10(uk2)
ggplot2::autoplot(tsLog) + labs(x ="Date", y = "Log Total", title="Plot the Log time series(1996-2006)") 

adf.test(tsLog)

kpss.test(tsLog)

tsDiffLog <- diff(log10(uk2),lag=1)
ggplot2::autoplot(tsDiffLog) + labs(x ="Date", y = "Log Total", title="Differenced Log Series (Total Air Travel) between (1996-2006)") 

adf.test(tsDiffLog)

kpss.test(tsDiffLog)

tsDiffLog <- diff(log10(uk2),lag=3)
ggplot2::autoplot(tsDiffLog) + labs(x ="Date", y = "Log Total", title="Differenced Log Series (Total Air Travel) between (1996-2006)") 

adf.test(tsDiffLog)
kpss.test(tsDiffLog)

# To examine which p and q values will be appropriate we need to run acf() and pacf() function
par(mfrow = c(1,2))
acf(tsDiffLog,main='ACF Total Travel from UK')
pacf(tsDiffLog,main='PACF Total Travel from UK')

md_ar <- ar(tsDiffLog)
md_ar

ARIMAfit <- arima(log(uk2),order=c(4,1,0))

summary(ARIMAfit)

ARIMAfit$model

ARIMAfit$series

# Predict for 2006 all quarters to answer the problem statement

forecast <- predict(ARIMAfit, n.ahead=4)
ts.plot(uk2,2.718^forecast$pred, log = "y", lty = c(1,3)) 

print(round(2.718^forecast$pred))  

#Explanations for the ts.plot arguments provided:

#2.718^forecast$pred: we are  undoing the log from the values.In order to do that, we need to find the log inverse of what we have got.
#i.e. log(forecast) = forecast$pred
#hence, forecast = e ^ forecast$pred
#e= 2.718 
#log = "y' is to plot on a logarithmic scale
#lty = c(1,3) will set the LineTYpe to 1 (for solid) for the original time series and 3 (for dotted) for the predicted time series.
#print(pred$pred) would give us log of the predicted values. print(2.718^pred$pred) would give us the actual predicted values.

# Accuracy of the ARIMA model
forecast::accuracy(ARIMAfit)

# simple exponential - models level
fit_hw <- HoltWinters(uk2, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit_hw <- HoltWinters(uk2, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit_hw <- HoltWinters(uk2)
fit_hw

summary(fit_hw)

# Predict for all quarters of 2004 and 2005 for accuracy comparison with uk2_test dataset
# Predict for 2006 Q1 and Q2 to answer the problem

forecast <- predict(fit_hw, n.ahead=4)
ts.plot(uk2,forecast, log = "y", lty = c(1,3)) 

print(round(forecast))

forecast::accuracy(fit_hw$fitted, uk2)

fit_ets <- ets(uk2)
summary(fit_ets)

# Predict for 2006 all quarters to answer the problem

fcast <- forecast(fit_ets, h=4)
plot(fcast)
print((fcast)) 

forecast::accuracy(fcast)

model_tbats <- tbats(uk2)
summary(model_tbats)

for_tbats <- forecast::forecast(model_tbats, h = 4)
df_tbats = as.data.frame(for_tbats)
df_tbats
print(df_tbats$`Point Forecast`)

ts.plot(uk2,forecast, log = "y", lty = c(1,3)) 

forecast::accuracy(model_tbats)

fit_auto <- auto.arima(uk2)
summary(fit_auto)

accuracy(fit_auto)

# Predict for 2006 all quarters to answer the problem

fcast_a <- forecast(fit_auto, h=4)
plot(fcast_a)
print((fcast_a)) 

print(round(forecast)) 
saveRDS(ARIMAfit, "C:\\Data_Science\\Certifications\\AnalytixLabs\\Data Science in R\\Data Science with R - Final Projects\\Time Series Forecast -  UK Air Travel\\TimeSeriesForecast-ARIMA.rda")
