#Loading the required libraries

library(dplyr)
library(forecast)
library(tseries)
require(graphics)

#loading the file into R
rm (list = ls(all=TRUE))
store_data <- read.csv("Global Superstore.csv", header = T, sep = ',', stringsAsFactors = F)

# Checking for NA Values
anyNA(store_data$Order.ID)
ColsWithNas<-colSums(is.na(store_data)) > 0
which(ColsWithNas==TRUE)
#Postal Code only contains NA values. This column is of not important for us. 

#Extracting month and year from Order date column
store_data$Order.Date <- as.Date(store_data$Order.Date, "%d-%m-%Y")
store_data$Month_Yr <- format(store_data$Order.Date, "%Y-%m")

#Retaining only needed column
store_data_short = subset(store_data, select = c(Segment,Market,Sales, Quantity, Profit, Month_Yr))

#Subsetting data frames for every combination of Market and Sales
table(store_data_short$Market,store_data_short$Segment )


df_list <- split(store_data_short, list(store_data$Market , store_data$Segment))
names(df_list) <- c("Africa_Consumer", "APAC_Consumer", "Canada_Consumer", "EMEA_Consumer", "EU_Consumer", "LATAM_Consumer", "US_Consumer",
                     "Africa_Corporate", "APAC_Corporate", "Canada_Corporate", "EMEA_Corporate", "EU_Corporate", "LATAM_Corporate", "US_Corporate",
                     "Africa_Home_Office", "APAC_Home_Office", "Canada_Home_Office", "EMEA_Home_Office", "EU_Home_Office", "LATAM_Home_Office", "US_Home_Office")

list2env(df_list, envir = .GlobalEnv)


#Finding the coefficient of variance based on profits column for all 21 dataframes

result <- data.frame(matrix(nrow = length(df_list), ncol = 2))
for (i in 1:length(df_list)) {
  
  profits <- setNames(aggregate(df_list[[i]]$Profit, by=list(df_list[[i]]$Month_Yr), 
                             FUN=sum, na.rm=TRUE),c("Month", "Profit"))
  profits_cv <- sd(profits$Profit)  / mean(profits$Profit)
  
  result[i, 1] <- names(df_list[i])
  result[i, 2] <- profits_cv
}
result[order(result$X2),]

# With lesser CVs , EU Consumer and APAC consumer segment market combinations turns to be the most consistent proftable segment
# Model builind for the below 4 time series
# Sales - EU Consumer
# Quantity - EU Consumer
# Sales - APAC Consumer
# Quantity - APAC Consumer

#First Model for Sales - EU Consumer
EU_Consumer_Sales <- EU_Consumer %>% select(Sales, Month_Yr) %>% arrange (Month_Yr)
EU_Consumer_Sales <- setNames(aggregate(EU_Consumer_Sales$Sales, by=list(EU_Consumer_Sales$Month_Yr), 
                                        FUN=sum, na.rm=TRUE),c("Month", "Sales"))

EU_Consumer_Sales$Month <- seq.int(nrow(EU_Consumer_Sales))

ts_EU_Consumer <- ts(EU_Consumer_Sales$Sales)
indata <- EU_Consumer_Sales[1:42,]
outdata <- EU_Consumer_Sales[43:48,]
timeser <- ts(indata$Sales)
plot(timeser)

#Smoothing the series - Moving Average Smoothing

w <-1
smoothedseries <- stats::filter(timeser, 
                         filter=rep(1/(1*w+1),(1*w+1)), 
                         method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries[w+2] - smoothedseries[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timeser)
diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- indata$Month
lines(smoothedseries, col="blue", lwd=2)

smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
colnames(smootheddf) <- c('Month', 'Sales')

#Now, let's fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month)  + cos(0.5*Month) * poly(Month,1)
            + Month , data=smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timeser-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
  armafit
  
#We'll check if the residual series is white noise
  
  resi <- local_pred-fitted(armafit)
  
  adf.test(resi,alternative = "stationary")
  kpss.test(resi)
  #Now, let's evaluate the model using MAPE
  #First, let's make a prediction for the last 6 months
  timevals_out <- outdata$Month
  
  global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
  fcast <- global_pred_out
  
  MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
  MAPE_class_dec
  
  class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
  plot(ts_EU_Consumer, col = "black")
  lines(class_dec_pred, col = "red")
  
# Arima fit

autoarima <- auto.arima(timeser)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#evaluate using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred, outdata[,2])[5]
MAPE_auto_arima

auto_arima_prd <- c(fitted(autoarima), ts(fcast_auto_arima$pred))
plot(ts_EU_Consumer, col = "black")
lines(auto_arima_prd, col = "red")
#######################################################333
# Building the model for EU Consumer Quantity 

EU_Consumer_Quantity <- EU_Consumer %>% select(Quantity , Month_Yr) %>% arrange (Month_Yr)
  EU_Consumer_Quantity  <- setNames(aggregate(EU_Consumer_Quantity $Quantity , by=list(EU_Consumer_Quantity $Month_Yr), 
                                          FUN=sum, na.rm=TRUE),c("Month", "Quantity "))
  
  EU_Consumer_Quantity$Month <- seq.int(nrow(EU_Consumer_Quantity ))
  
  ts_EU_Consumer <- ts(EU_Consumer_Quantity$Quantity )
  indata <- EU_Consumer_Quantity [1:42,]
  outdata <- EU_Consumer_Quantity [43:48,]
  timeser <- ts(indata$Quantity )
  plot(timeser)
  
  #Smoothing the series - Moving Average Smoothing
  
  w <-1
  smoothedseries <- stats::filter(timeser, 
                                  filter=rep(1/(1*w+1),(1*w+1)), 
                                  method='convolution', sides=2)
  
  #Smoothing left end of the time series
  
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  #Smoothing right end of the time series
  
  n <- length(timeser)
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  
  #Plot the smoothed time series
  
  timevals_in <- indata$Month
  lines(smoothedseries, col="blue", lwd=2)
  
  smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
  colnames(smootheddf) <- c('Month', 'Quantity')
  
  #Now, let's fit a multiplicative model with trend and seasonality to the data
  #Seasonality will be modeled using a sinusoid function
  
  lmfit <- lm(Quantity ~ sin(0.5*Month)  + cos(0.5*Month) * poly(Month,1)
              + Month , data=smootheddf)
  global_pred <- predict(lmfit, Month=timevals_in)
  summary(global_pred)
  lines(timevals_in, global_pred, col='pink', lwd=2)
  
  #Now, let's look at the locally predictable series
  #We will model it as an ARMA series
  
  local_pred <- timeser-global_pred
  plot(local_pred, col='red', type = "l")
  acf(local_pred)
  acf(local_pred, type="partial")
  armafit <- auto.arima(local_pred)
  
  tsdiag(armafit)
  armafit
  
  #We'll check if the residual series is white noise
  
  resi <- local_pred-fitted(armafit)
  
  adf.test(resi,alternative = "stationary")
  kpss.test(resi)
  #Now, let's evaluate the model using MAPE
  #First, let's make a prediction for the last 6 months
  timevals_out <- outdata$Month
  
  global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
  fcast <- global_pred_out
  
  MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
  MAPE_class_dec
  
  class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
  plot(ts_EU_Consumer, col = "black")
  lines(class_dec_pred, col = "red")

  
  # Arima fit
  
  autoarima <- auto.arima(timeser)
  autoarima
  tsdiag(autoarima)
  plot(autoarima$x, col="black")
  lines(fitted(autoarima), col="red")
  
  #evaluate using MAPE
  fcast_auto_arima <- predict(autoarima, n.ahead = 6)
  
  MAPE_auto_arima <- accuracy(fcast_auto_arima$pred, outdata[,2])[5]
  MAPE_auto_arima
  
  auto_arima_prd <- c(fitted(autoarima), ts(fcast_auto_arima$pred))
  plot(ts_EU_Consumer, col = "black")
  lines(auto_arima_prd, col = "red")

##############################################################################3
#Building the model for apac sales
  
  APAC_Consumer_Sales <- APAC_Consumer %>% select(Sales, Month_Yr) %>% arrange (Month_Yr)
  APAC_Consumer_Sales <- setNames(aggregate(APAC_Consumer_Sales$Sales, by=list(APAC_Consumer_Sales$Month_Yr), 
                                          FUN=sum, na.rm=TRUE),c("Month", "Sales"))
  
  APAC_Consumer_Sales$Month <- seq.int(nrow(APAC_Consumer_Sales))
  
  ts_APAC_Consumer <- ts(APAC_Consumer_Sales$Sales)
  indata <- APAC_Consumer_Sales[1:42,]
  outdata <- APAC_Consumer_Sales[43:48,]
  timeser <- ts(indata$Sales)
  plot(timeser)
  
  #Smoothing the series - Moving Average Smoothing
  
  w <-1
  smoothedseries <- stats::filter(timeser, 
                                  filter=rep(1/(1*w+1),(1*w+1)), 
                                  method='convolution', sides=2)
  
  #Smoothing left end of the time series
  
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  #Smoothing right end of the time series
  
  n <- length(timeser)
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  
  #Plot the smoothed time series
  
  timevals_in <- indata$Month
  lines(smoothedseries, col="blue", lwd=2)
  
  smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
  colnames(smootheddf) <- c('Month', 'Sales')
  
  #Now, let's fit a additive model with trend and seasonality to the data
  #Seasonality will be modeled using a sinusoid function
  
  lmfit <- lm(Sales ~ sin(0.5*Month)  + cos(0.5*Month) * poly(Month,1)
              + Month , data=smootheddf)
  global_pred <- predict(lmfit, Month=timevals_in)
  summary(global_pred)
  lines(timevals_in, global_pred, col='red', lwd=2)
  
  #Now, let's look at the locally predictable series
  #We will model it as an ARMA series
  
  local_pred <- timeser-global_pred
  plot(local_pred, col='blue', type = "l")
  acf(local_pred)
  acf(local_pred, type="partial")
  armafit <- auto.arima(local_pred)
  
  tsdiag(armafit)
  armafit
  
  #We'll check if the residual series is white noise
  
  resi <- local_pred-fitted(armafit)
  
  adf.test(resi,alternative = "stationary")
  kpss.test(resi)
  #Now, let's evaluate the model using MAPE
  #First, let's make a prediction for the last 6 months
  timevals_out <- outdata$Month
  
  global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
  fcast <- global_pred_out
  
  MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
  MAPE_class_dec
  
  class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
  plot(ts_APAC_Consumer, col = "black")
  lines(class_dec_pred, col = "red")
  
  # Arima fit
  
  autoarima <- auto.arima(timeser)
  autoarima
  tsdiag(autoarima)
  plot(autoarima$x, col="black")
  lines(fitted(autoarima), col="red")
  
  #evaluate using MAPE
  fcast_auto_arima <- predict(autoarima, n.ahead = 6)
  
  MAPE_auto_arima <- accuracy(fcast_auto_arima$pred, outdata[,2])[5]
  MAPE_auto_arima
  
  auto_arima_prd <- c(fitted(autoarima), ts(fcast_auto_arima$pred))
  plot(ts_APAC_Consumer, col = "black")
  lines(auto_arima_prd, col = "red")
  #######################################################333
  # Building the model for EU Consumer Quantity 
  
  APAC_Consumer_Quantity <- APAC_Consumer %>% select(Quantity , Month_Yr) %>% arrange (Month_Yr)
  APAC_Consumer_Quantity  <- setNames(aggregate(APAC_Consumer_Quantity$Quantity , by=list(APAC_Consumer_Quantity$Month_Yr), 
                                              FUN=sum, na.rm=TRUE),c("Month", "Quantity "))
  
  APAC_Consumer_Quantity$Month <- seq.int(nrow(APAC_Consumer_Quantity ))
  
  ts_APAC_Consumer <- ts(APAC_Consumer_Quantity$Quantity )
  indata <- APAC_Consumer_Quantity [1:42,]
  outdata <- APAC_Consumer_Quantity [43:48,]
  timeser <- ts(indata$Quantity )
  plot(timeser)
  
  #Smoothing the series - Moving Average Smoothing
  
  w <-1
  smoothedseries <- stats::filter(timeser, 
                                  filter=rep(1/(1*w+1),(1*w+1)), 
                                  method='convolution', sides=2)
  
  #Smoothing left end of the time series
  
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  #Smoothing right end of the time series
  
  n <- length(timeser)
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  
  #Plot the smoothed time series
  
  timevals_in <- indata$Month
  lines(smoothedseries, col="blue", lwd=2)
  
  smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries)))
  colnames(smootheddf) <- c('Month', 'Quantity')
  
  #Now, let's fit a multiplicative model with trend and seasonality to the data
  #Seasonality will be modeled using a sinusoid function
  
  lmfit <- lm(Quantity ~ sin(0.5*Month)  + cos(0.5*Month) * poly(Month,1)
              + Month , data=smootheddf)
  global_pred <- predict(lmfit, Month=timevals_in)
  summary(global_pred)
  lines(timevals_in, global_pred, col='pink', lwd=2)
  
  #Now, let's look at the locally predictable series
  #We will model it as an ARMA series
  
  local_pred <- timeser-global_pred
  plot(local_pred, col='red', type = "l")
  acf(local_pred)
  acf(local_pred, type="partial")
  armafit <- auto.arima(local_pred)
  
  tsdiag(armafit)
  armafit
  
  #We'll check if the residual series is white noise
  
  resi <- local_pred-fitted(armafit)
  
  adf.test(resi,alternative = "stationary")
  kpss.test(resi)
  #Now, let's evaluate the model using MAPE
  #First, let's make a prediction for the last 6 months
  timevals_out <- outdata$Month
  
  global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))
  fcast <- global_pred_out
  
  MAPE_class_dec <- accuracy(fcast,outdata[,2])[5]
  MAPE_class_dec
  
  class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
  plot(ts_APAC_Consumer, col = "black")
  lines(class_dec_pred, col = "red")
  
  
  # Arima fit
  
  autoarima <- auto.arima(timeser)
  autoarima
  tsdiag(autoarima)
  plot(autoarima$x, col="black")
  lines(fitted(autoarima), col="red")
  
  #evaluate using MAPE
  fcast_auto_arima <- predict(autoarima, n.ahead = 6)
  
  MAPE_auto_arima <- accuracy(fcast_auto_arima$pred, outdata[,2])[5]
  MAPE_auto_arima
  
  auto_arima_prd <- c(fitted(autoarima), ts(fcast_auto_arima$pred))
  plot(ts_EU_Consumer, col = "black")
  lines(auto_arima_prd, col = "red")