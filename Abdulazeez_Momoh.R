# Loading required Libraries
library(datarium)
library(qqplotr)
library(RVAideMemoire)
library(car)
library(corrplot)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tseries)
library(tidyverse)
library(moments)
library(dplyr)
library(ggfortify)
library(forecast)
library(moments)
library(modeest)
library(TTR)



# Read in data
worlddevelopment <- read.csv("P_Data_Extract_From_World_Development_Indicatorsss.csv", header= TRUE)


# Data exploration
str(worlddevelopment)
names(worlddevelopment) #We can see that the Dataset has 15 columns
glimpse(worlddevelopment)
head(worlddevelopment)
summary(worlddevelopment)

# Checking missing values 
print("Position of missing values by column wise----")
sapply(worlddevelopment, function(x) which(is.na(x)))

# Removing the rows that contain missing values
worlddevelopment <- na.omit(worlddevelopment)
print("Position of missing values by column wise----")
sapply(worlddevelopment, function(x) which(is.na(x)))

# Renaming columns for easy coding

worlddevelopment<- worlddevelopment %>%
  rename(
    'year' = 'Time',
    'country' = 'Country.Name',
    'gdp_current' = 'GDP..current.US....NY.GDP.MKTP.CD.',
    'trade'='Trade..Sum.of.Imports.and.Exports.of.Goods.and.Services.',
    'trade_gdp'='Trade....of.GDP...NE.TRD.GNFS.ZS.',
    'net_trade'='Net.trade.in.goods.and.services..BoP..current.US....BN.GSR.GNFS.CD.',
    'import_goods_services' = 'Imports.of.goods.and.services..current.US....NE.IMP.GNFS.CD.',
    'export_goods_services' = 'Exports.of.goods.and.services..current.US....NE.EXP.GNFS.CD.',
    'goods_export' = 'Goods.exports..BoP..current.US....BX.GSR.MRCH.CD.',
    'service_export' = 'Service.exports..BoP..current.US....BX.GSR.NFSV.CD.',
    'service_import' = 'Service.imports..BoP..current.US....BM.GSR.NFSV.CD.',
    'import_goods_services_gdp' = 'Imports.of.goods.and.services....of.GDP...NE.IMP.GNFS.ZS.',
    'trade_in_services' = 'Trade.in.services....of.GDP...BG.GSR.NFSV.GD.ZS.',
    'merchandise_trade' = 'Merchandise.trade....of.GDP...TG.VAL.TOTL.GD.ZS.',
    'export_goods_services_annual' = 'Exports.of.goods.and.services..annual...growth...NE.EXP.GNFS.KD.ZG.',
    'import_goods_services_annual' = 'Imports.of.goods.and.services..annual...growth...NE.IMP.GNFS.KD.ZG.',
    'gross_savings' = 'Gross.savings..current.US....NY.GNS.ICTR.CD.',
    'gross_savings_gdp' = 'Gross.savings....of.GDP...NY.GNS.ICTR.ZS.'
  )

# Extracting numerical variables from and store them in a new dataset
worlddevelopment_reduced <- worlddevelopment[-c(2,3,4)]
head(worlddevelopment_reduced, 7)


#QUESTION 1. Do a comprehensive descriptive statistical analysis

apply(worlddevelopment_reduced, 2, mean)
apply(worlddevelopment_reduced, 2, median)

summary(worlddevelopment_reduced)


#To determine the Standard deviation, Skewness and Kurtosis of the dataset

#Standard deviation
sapply(worlddevelopment_reduced, sd)

#Skewness
skewness(worlddevelopment_reduced)

 #Kurtosis 
kurtosis(worlddevelopment_reduced)

#QUESTION 2. Correlation analysis for the indicators and evaluate the results

#Correlation matrix between all variables
cor(worlddevelopment_reduced)

#Viaulize the Correlation Matrix

corrplot(cor(worlddevelopment_reduced), type="upper")
corrplot(cor(worlddevelopment_reduced), method="number", type="upper")

#QUESTION 3 - Regression Analysis

# 3.1 - Simple Linear Regression

# 1 - To determine how the export of goods and services affect the gdp = 0.88 (High Correlation)
model_1 <- lm(gdp_current ~ export_goods_services, worlddevelopment_reduced)
summary.lm(model_1)

#Result
#Both coefficients are significant and the SLR equation will be;
#gdp_current = -4.835e+11 + 5.954e+00 * export_goods_services
#R-squared = 0.7736 and this means that this equation export_goods_services 
#can predict about 77% of the entire variability in the gdp_current

#Linearity
#Vsualising the fitted regression line we should draw the scatter plot;
plot(gdp_current~export_goods_services, worlddevelopment_reduced,
     col = "blue",
     main = "Regression: export_goods_services & gdp_current",
     xlab = "gdp_current",
     ylab = "export_goods_services")

#Including the regression line to the plot
abline(model_1, col="red")

#Residuals independence
plot(model_1, 1)

#Normality of residuals
plot(model_1, 2)

#Equal variances of the residuals
plot(model_1, 3)

#We can use this equation to predict a gdp_cuurent based on export_goods_services
#Both coefficients are significant and the SLR equation will be
gdp_current_pred = -4.835e+11 + 5.954e+00 * 1.86E+12
gdp_current_pred


# 2 - High correlation exist between merchandise trade and trade_gdp = 0.97 (High Correlation)
model_2 <- lm(trade_gdp~merchandise_trade, worlddevelopment_reduced)
summary.lm(model_2)

#Result
#Both coefficients are significant and the SLR equation will be;
#gdp_current = 5.22008 + 1.18657 * merchandise_trade
#R-squared = 0.9366 and this means that this equation export_goods_services 
#can predict about 93% of the entire variability in the trade_gdp

#Linearity
#Vsualising the fitted regression line we should draw the scatter plot;
plot(trade_gdp~merchandise_trade, worlddevelopment_reduced,
     col = "blue",
     main = "Regression: merchandise_trade & trade_gdp",
     xlab = "merchandise_trade",
     ylab = "trade_gdp")

#Including the regression line to the plot
abline(model_2, col="red")

#Residuals independence
plot(model_2, 1)

#Normality of residuals
plot(model_2, 2)

#Equal variances of the residuals
plot(model_2, 3)

#We can use this equation to predict a gdp_cuurent based on export_goods_services
#Both coefficients are significant and the SLR equation will be
trade_gdp_pred = 5.22008 + 1.18657 * 21.58074738
trade_gdp_pred


# 3.2 - Multiple Linear Regression
#1 #To determine export_goods_services and merchandise trade affect the gdp
model_3 <-lm(gdp_current~export_goods_services+import_goods_services, worlddevelopment_reduced)
summary.lm(model_3)

#Result
#The combination of variables in the model produced Adjusted R-squared of 0.9763

#2 #To determine export_goods_services and merchandise trade affect the gdp
model_4 <-lm(gdp_current ~ export_goods_services + import_goods_services + gross_savings, worlddevelopment_reduced)
summary.lm(model_4)

#Result
#The combination of variables in the model produced Adjusted R-squared of 0.997 
#and it is larger than the previous MLR model with Ajusted R-squared of 0.9763


#In order to draw the scatter plot matrix,see the indices of the 4 variables
#to select the columns
data.frame(colnames(worlddevelopment_reduced))
pairs(worlddevelopment_reduced[,c(22,20,10,19)], lower.panel = NULL, PCH = 19, CEX = 0.2)

#Residuals independence
plot(model_4, 1)

#Normality of residuals
plot(model_4, 2)

#Equal variances of the residuals
plot(model_4, 3)

#Using the Car package to see if the variance inflation factor (VIF) measures
#From the result all the values are less than 5 and we can conclude there is no collinearity between Independent Variables 

#Report the results;All 5 Assumptions were approved, and we can confirm our fitted regression line as follows
#We can usew this equation to predict a gdp_current on its
#export_goods_services, import_goods_services and merchandise_trade
#gdp_current_pred = -4.835e+11 + 5.954e+00 * 1.86E+12 + 5.22008 + 1.18657 * 21.58074738
gdp_current_pred = -4.835e+11 + 5.954e+00 * 1.86E+12 + 5.22008 + 1.18657 * 21.58074738
gdp_current_pred



##Question 4

#Time Series

#Group by year for the analysis

worlddevelopment_grouped <- aggregate(gdp_current ~ year, data=worlddevelopment_reduced, FUN =mean, na.rm = TRUE)
head(worlddevelopment_grouped)

#Determine and plot the Time series
gdp_current_timeseries <- ts(worlddevelopment_grouped$gdp_current, start = c(2010))
gdp_current_timeseries
plot.ts(gdp_current_timeseries)


#Decomposing Non-seasonal Data
gdp_current_timeseries_SMA3 <- SMA(gdp_current_timeseries, n=3)
plot.ts(gdp_current_timeseries_SMA3)


#Forecast using Exponential Soothing
gdp_current_timeseries_forecasts <- HoltWinters(gdp_current_timeseries,gamma=FALSE)
gdp_current_timeseries_forecasts
#Output tells us taht the estimated value of the alpha parameter is 1

gdp_current_timeseries_forecasts$SSE

plot(gdp_current_timeseries_forecasts)
#The plot shows the original time series in bloack and the forecast as a red line.
#the forecast timeseries is much smoother when compared to that of the original data

#sum of squared errors(SSE)
#The forecast errors for the time period covered by our original time series
gdp_current_timeseries_forecasts$SSE

#To make forecast with the initial value of the level set to 1.649818e+23(first value in the data)
#b.start = second value- first value of the series
HoltWinters(gdp_current_timeseries, gamma = FALSE, l.start=1, b.start=0.4499125)

#To make a forecast of gdp from the years 2019 to 2029 using forecast
gdp_current_timeseries_forecasts2 <- forecast(gdp_current_timeseries_forecasts, h=10)
gdp_current_timeseries_forecasts2

#plotting the predictions made
plot(gdp_current_timeseries_forecasts2)

#calculating the correlogram of the in-sample forecast errors for the data for logs1-20
acf(gdp_current_timeseries_forecasts2$residuals, lag.max = 20, na.action = na.pass)

Box.test(gdp_current_timeseries_forecasts2$residuals, lag=7, type="Ljung-Box")

plot.ts(gdp_current_timeseries_forecasts2$residuals)

plotForecastErrors <- function(forecasterrors)

{
    # make a histogram of the forecast errors:
    mybinsize <- IQR(forecasterrors)/4
    mysd    <- sd(forecasterrors)
    mymin <- min(forecasterrors) - mysd*5
    mymax <- max(forecasterrors) + mysd*3
    # generate normally distributed data with mean 0 and standard deviation mysd
    mynorm <- rnorm(10000, mean=0, sd=mysd)
    mymin2 <- min(mynorm)
    mymax2 <- max(mynorm)
    if (mymin2 < mymin) { mymin <- mymin2 }
    if (mymax2 > mymax) { mymax <- mymax2 }
    # make a red histogram of the forecast errors, with the normally distributed data overlaid:
    mybins <- seq(mymin, mymax, mybinsize)
    hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
    # freq=FALSE ensures the area under the histogram = 1
    # generate normally distributed data with mean 0 and standard deviation mysd
    myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
    # plot the normal curve as a blue line on top of the histogram of forecast errors:
    points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
  }

gdp_current_timeseries_forecasts2$residuals <-
  gdp_current_timeseries_forecasts2$residuals[!is.na(gdp_current_timeseries_forecasts2$residuals)]
  
plotForecastErrors(gdp_current_timeseries_forecasts2$residuals)







