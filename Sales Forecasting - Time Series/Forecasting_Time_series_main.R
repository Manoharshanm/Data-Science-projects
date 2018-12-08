#####################################################################
#      Retail-Giant Sales Forecasting Case Study Assignment         #
#####################################################################

# Group members 
# Shivam Kakkar (Facilitator) - Roll Number - DDA1730346
# Ashwin Suresh
# Manohar Shanmugasundaram
# P Sai Prathyusha

################################
# Loading the required libraries
################################

library(ggplot2)
library(forecast)
library(dplyr)
library(gridExtra)
library(raster)
library(tseries)
library(stats)

##################
# Data Preparation
##################

# Reading the global superstore data from csv file

gStore<- read.csv("Global Superstore.csv")

# Check the structure of the data

str(gStore)
dim(gStore)  #51290 , 24

# Count of NAs in every column

colSums(is.na(gStore))  # All the NA are in Postal Code

# Convert the Order.Date to Date format

gStore$Order.Date<-as.Date(gStore$Order.Date,"%d-%m-%Y")

# Sort the data, as the order date is not in chronological order. 
# The data should be changed to chronological order to do time series analysis
gStore <- gStore[order(gStore$Order.Date,gStore$Market,gStore$Segment),]

# Convert the Oder.Date to 'YearMonth' format

gStore$Order.Date<-format(gStore$Order.Date, "%Y%m")
gStore$Order.Date<-as.numeric(gStore$Order.Date)

# Creating 21 buckets respective to every "Market and Segment combination"
# For example : Africa - Consumer, Africa - Corporate ... 
# There are 21 combnations like this
# This will create a list of 21 data frames respectively for each combination

buckets_21 <- split.data.frame(gStore , list(gStore$Segment , gStore$Market)) #21

# Initializing a data frame "COV_values" , it will store "Coefficient of Variation" for 
# every Markey segment combnation

COV_values <- data.frame(market_segment = character(),
                         Coefficient_of_Variation = numeric(),
                         stringsAsFactors = FALSE)

# Now iterating through all the 21 buckets.
# For every bucket we will aggregate "Profit" , "Sales" & "Quantity" at monthly level
# Coefficient of variation is calculated for every bucket:
# For example:
# If i am iterating over the bucket say : "Market as: Africa and Segment as: Consumer"
# A new dataframe will be created with the name "Africa_consumer"  i.e "Market_segment"
# And each such dataframe will have monthly aggregated data.
# And COV_values dataframe will be populated with the coefficient of variation for every "Market_segment"

# Initialize an empty data frame

sales_summary_df = data.frame()

for (bucket in buckets_21) {
  
  df <- as.data.frame(bucket)
  names(df) <- colnames(gStore)
  
  market <- as.character(unique(df$Market))
  segment <- as.character(unique(df$Segment))
  market_segment <- paste(market , segment , sep = '_')
  
  df <- df %>% group_by(Order.Date) %>% 
                  summarise(total_qty = sum(Quantity),
                  total_sales_amount = sum(Sales), 
                  total_profit = sum(Profit))
  
  # Calculating coefficient of variation using the "cv" function of raster package
  
  COV <- cv(df$total_profit)
  
  # Updating the COV value respective to the <market_segment> in the COV_values data frame
  
  COV_values[nrow(COV_values) + 1,  ] <- c(market_segment , COV)
  
  # Creating a dynamic variable with the name as <market>_<segment>, pointing to a data frame
  # contains the monthly aggregated data
  
  assign(market_segment , df)
  
  # Combining the aggregated data onto a single data set to do EDA analysis
  df$Market_segment <- market_segment
  sales_summary_df <- rbind(sales_summary_df,df)
}
  
##############################
# Exploratory Data Analysis  #
##############################

#####################
# Univariate Analysis
#####################

ggplot(sales_summary_df, aes(x=as.factor(Market_segment))) + geom_bar(fill="brown") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Market_Segment", y="Month record count", title = "Monthly sales record for Market Segments")

####################
# Bivariate Analysis
####################

plot1 <- sales_summary_df %>% group_by(Market_segment) %>% 
  summarise(total_sales = sum(total_sales_amount)) %>%
  ggplot(aes(x=Market_segment, y=total_sales)) + geom_col(fill = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Market Segment", y="Total Sales", title = "Total sales for each Market Segments")

plot2 <- sales_summary_df %>% group_by(Market_segment) %>% 
  summarise(total_quantity = sum(total_qty)) %>%
  ggplot(aes(x=Market_segment, y=total_quantity)) + geom_col(fill = "blue") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Market Segment", y="Total Quantity", title = "Total quantity sold for each Market Segments")

plot3 <- sales_summary_df %>% group_by(Market_segment) %>% 
  summarise(total_sale_profit = sum(total_profit)) %>%
  ggplot(aes(x=Market_segment, y=total_sale_profit)) + geom_col(fill = "brown") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Market Segment", y="Total Profit", title = "Total profit for Market Segments")

grid.arrange(plot1,plot2,plot3, ncol = 3, nrow=1, top = "Retail-Giant Sales & Profit Summary")

##########################
# Coefficient of Variation
##########################

# all the 21 followng variables are created dynamically in the loop only:

# Africa_consumer , Africa_Corporate, Africa_Home Office
# APAC_consumer , APAC_corporate , APAC_Home Office
# Canada_consumer , Canada_Corporate , Canada_Home Office
# ... and so on

COV_values$Coefficient_of_Variation <- as.numeric(COV_values$Coefficient_of_Variation)

# market_segment that has least coefficient of variation

min_COV <- min(COV_values$Coefficient_of_Variation)
index_min_COV <- which(COV_values$Coefficient_of_Variation == min_COV)
COV_values$market_segment[index_min_COV] #"EU_Consumer"

# market_segment that has second least coefficeint of variation
second_min_COV <- min(COV_values$Coefficient_of_Variation[-index_min_COV])
index_second_min_COV <- which(COV_values$Coefficient_of_Variation == second_min_COV)
COV_values$market_segment[index_second_min_COV] #"APAC_Consumer"

# Now we can see least coefficient of variation is coming for :
# EU Consumer
# APAC consumer

ggplot(COV_values , aes(x = factor(market_segment) , y = Coefficient_of_Variation)) + 
  geom_col(fill= "green") +
  geom_text(aes(label=round(Coefficient_of_Variation, digits = 2)),vjust=-1) + labs(x = "Market_Segment") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#######################################################
# Now we will use the following datasets for analysis 
# and Model Building process.

# EU_Consumer_Sales 
# EU_Consumer_Qty
# APAC_Consumer_Sales
# APAC_Consumer_Qty

#######################################################

EU_Consumer_Sales <- EU_Consumer$total_sales_amount #48
EU_Consumer_Qty <- EU_Consumer$total_qty #48

APAC_Consumer_Sales <- APAC_Consumer$total_sales_amount # 48
APAC_Consumer_Qty <- APAC_Consumer$total_qty #48

#########################
# Profit Segment Analysis
#########################

# Plot 1 for Sales summary for EU Consumer

plot_yr1 <- EU_Consumer %>% 
  group_by(year=round(Order.Date/100)) %>% 
  summarise(total_sales = sum(total_qty)) %>%
  ggplot(aes(x=as.factor(year), y=total_sales)) + geom_col(fill = "brown") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Year", y="Total Quantity", title = "Total sales for EU Consumer per year")

# Plot 2 for Quantity summary for EU Consumer

plot_yr2 <- EU_Consumer %>% 
  group_by(year=round(Order.Date/100)) %>% 
  summarise(total_quantity = sum(total_qty)) %>%
  ggplot(aes(x=as.factor(year), y=total_quantity)) + geom_col(fill = "brown") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Year", y="Total Quantity", title = "Total quantity sold for EU Consumer per year")

# Plot 3 for Sales summary for APAC Consumer

plot_yr3 <- APAC_Consumer %>%
  group_by(year=round(Order.Date/100)) %>% 
  summarise(total_sales = sum(total_sales_amount)) %>%
  ggplot(aes(x=as.factor(year), y=total_sales)) + geom_col(fill = "brown") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Year", y="Total Sales", title = "Total sales for APAC Consumer per year")

# Plot 4 for Quantity summary for APAC Consumer

plot_yr4 <- APAC_Consumer %>% 
  group_by(year=round(Order.Date/100)) %>% 
  summarise(total_quantity = sum(total_qty)) %>%
  ggplot(aes(x=as.factor(year), y=total_quantity)) + geom_col(fill = "brown") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Year", y="Total Quantity", title = "Total quantity sold for APAC Consumer per year")

grid.arrange(plot_yr1,plot_yr2,plot_yr3,plot_yr4, ncol = 2, nrow=2, 
             top = "Sale and Quantity summary for EU and APAC Consumer segments")


# Following  Utility functions we will use in our "Model Building process"

##################
#Utility functions
##################

########################################################################################################

# This function will create a time series of the input data and returna time series object

create_timeseries <- function(input_data) {
  return (ts(input_data))
}

# This function will smoothen the given timeseries

smoothen_timeseries <- function(timeser) {
  w <-1
  smoothed_series <- stats::filter(timeser, 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)
  
  # Smoothing the left end of the time series
  
  diff <- smoothed_series[w+2] - smoothed_series[w+1]
  for (i in seq(w,1,-1)) {
    smoothed_series[i] <- smoothed_series[i+1] - diff
  }
  
  # Smoothing the right end of the time series
  
  n <- length(timeser)
  
  diff <- smoothed_series[n-w] - smoothed_series[n-w-1]
  for (i in seq(n-w+1, n)) {
    smoothed_series[i] <- smoothed_series[i-1] + diff
  }  
  
  return (smoothed_series)
}

# This function will build a linear model on the smoothened data frame 

build_linear_model <- function(smoothened_df , formula) {
  lmfit <- lm(formula ,data = smoothened_df)  
}

# This function will predict the values using the built model

predict_values <- function(model , df) {
  return (predict(model,df))
}

########################################################################################################

#########################################
# Model Building I and Model Evaluation I
#########################################

# As part of Model Bulding I and Model Evaluation I we have used 
# Classical Decomposition  & auto.arima process
# MAPE is the metric that we have used for Model Evaluation

#############################
# 1. Using EU_Consumer_Sales
#############################

# Using first 42 records of "EU_COnsumer_Sales"  to build a training time series object

EU_Consumer_Sales_ts <- create_timeseries(EU_Consumer_Sales[1:42])

# Plotting the build timeseries object, built in previous step i.e EU_Consumer_Sales_ts

plot(EU_Consumer_Sales_ts , xlab = "Month" , ylab = "Total Sales Amount" , col = "blue" , 
     main = "Sales trend from Jan 2011 - Dec 2014 in EU for Consumer segment")

# Smoothening the timeseries : i.e "EU_Consumer_Sales_ts"

EU_Consumer_Sales_ts_smoothen <- smoothen_timeseries(EU_Consumer_Sales_ts)

# Plotting the smoothened times series over the existing plot of timeseries object

lines(EU_Consumer_Sales_ts_smoothen, col="blue", lwd=2)

# Creating a dataframe from the smoothened time series object i.e EU_Consumer_Sales_ts_smoothen

EU_Consumer_Sales_ts_smoothen_df <- as.data.frame(cbind(seq(1:42), as.vector(EU_Consumer_Sales_ts_smoothen)))

# Updating the column names to 'Month' and 'Sales'

colnames(EU_Consumer_Sales_ts_smoothen_df) <- c('Month', 'Sales')

# Building a linear model that can be used to predict the values globally using the following formula
# We are trying to capture seasonality using sinusoidal function.

formula <- as.formula(Sales ~ sin(0.483*Month)  + cos(0.4*Month)* poly(Month,1) + Month) # 22.28

# The built model is stored in lm_fit

lm_fit <- build_linear_model(EU_Consumer_Sales_ts_smoothen_df , formula)

# Predicting the values of EU_Consumer_Sales using the built model

global_pred <- predict(lm_fit, Month=EU_Consumer_Sales_ts_smoothen_df$Month)

summary(global_pred) #

lines(global_pred, col='red', lwd=2)

# build an arma series for local predictability

local_pred <- EU_Consumer_Sales_ts - global_pred

# Plotting the local predictability pattern

plot(local_pred, col='red', type = "l")

# ACF PLOT

acf(local_pred)

# PACF plot

acf(local_pred, type="partial")

armafit <- auto.arima(local_pred)  

# Diagnostics

tsdiag(armafit)

armafit #ARIMA(0,0,0) with zero mean , AIC=901.28   AICc=901.38   BIC=903.02

# Verify if the residual series is white noise or not using
# Dickey fuller test and kpss

resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary") 

kpss.test(resi)

# residual series is stationary 

#############################################
# Model Evaluation part for EU_COnsumer_sales
#############################################

# To evaluate the model, we are using last 6 months data

options(warn=-1)
global_pred_out <- predict(lm_fit , data.frame(Month = seq(43,48)))
options(warn=1)

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(global_pred_out,EU_Consumer_Sales[43:48])[5]
MAPE_class_dec #22.28

# Hence the mape of manually built model is 22.28

##################################################
# Following is Auto ARIMA for EU_Consumer_Sales_ts
##################################################

autoarima <- auto.arima(EU_Consumer_Sales_ts)
autoarima #ARIMA(2,1,0) , AIC=897.67   AICc=898.32   BIC=902.81

tsdiag(autoarima) 
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

# Verify if the residual series is white noise or not using
# Dickey fuller test and kpss

resi_auto_arima <- EU_Consumer_Sales_ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

# residual series is stationary

# Here we are evaluating the model built using auto.arima
# We are using last 6 months of data

fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,EU_Consumer_Sales[43:48])[5]
MAPE_auto_arima # 28.92

# Wth Auto arima MAPE value is coming as 28.92

# Plotting the predictions along with original values, to get a visual feel of the fit.

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(EU_Consumer_Sales_ts, col = "black")
lines(auto_arima_pred, col = "red")

##############################################################################################################
# Since our manual model is better than arma model we will use our manual model forecast the EU_Consumer sales 
# for next 6 months: Our Manual mmodel is less than the Mape of the model with auto.arima : 22.28 < 28.92
##############################################################################################################

# Forecasting the sales of next 6 months EU_Consumer Sales using our manual model.

options(warn = -1)
fcast_EU_sales_next_6_months <- predict(lm_fit , data.frame(Month = seq(49,54)))
options(warn = 1)

round(fcast_EU_sales_next_6_months) # 54656 51821 47451 42396 37678 34306

#############################################################################################################
#############################################################################################################

###########################
# 2. Using EU_Consumer_Qty
##########################

# Using first 42 records of "EU_COnsumer_Qty"  to build a training time series object

EU_Consumer_Qty_ts <- create_timeseries(EU_Consumer_Qty[1:42])

# Plotting the build timeseries object, built in previous step i.e EU_Consumer_Qty_ts

plot(EU_Consumer_Qty_ts , xlab = "Month" , ylab = "Total Qty" , col = "blue" , 
     main = "Qty trend from Jan 2011 - Dec 2014 in EU for Consumer segment")

# Smoothening the timeseries : i.e "EU_Consumer_Qty_ts"

EU_Consumer_Qty_ts_smoothen <- smoothen_timeseries(EU_Consumer_Qty_ts)

# Plotting the smoothened times series over the existing plot of timeseries object

lines(EU_Consumer_Qty_ts_smoothen, col="blue", lwd=2)

# Creating a dataframe from the smoothened time series object i.e EU_Consumer_Qty_ts_smoothen

EU_Consumer_Qty_ts_smoothen_df <- as.data.frame(cbind(seq(1:42), as.vector(EU_Consumer_Qty_ts_smoothen)))

# Updating the column names to 'Month' and 'Qty'

colnames(EU_Consumer_Qty_ts_smoothen_df) <- c('Month', 'Qty')

# Building a linear model that can be used to predict the values globally using the following formula
# We are trying to capture seasonality using sinusoidal function.

formula <- as.formula(Qty ~ sin(0.60*Month) * poly(Month,3) + cos(0.6*Month) * poly(Month,3) + Month)

# The built model is stored in lm_fit

lm_fit <- build_linear_model(EU_Consumer_Qty_ts_smoothen_df , formula)

# Predicting the values of EU_Consumer_Qty using the built model

global_pred <- predict(lm_fit, Month=EU_Consumer_Qty_ts_smoothen_df$Month)
summary(global_pred)
lines(global_pred, col='red', lwd=2)

# build an arma series for local predictability

local_pred <- EU_Consumer_Qty_ts - global_pred

# Plotting the local predictability pattern

plot(local_pred, col='red', type = "l")

# ACF PLOT

acf(local_pred)

# PACF plot

acf(local_pred, type="partial")

armafit <- auto.arima(local_pred) 

# Diagnostics

tsdiag(armafit)

armafit #ARIMA(2,0,1) with zero mean  , # AIC=496.08   AICc=497.16   BIC=503.03

# Verifying if the residual series is white noise
# using Dickey fuller and kpss test

resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary") 

kpss.test(resi)

# The residual series is stationary

###########################################
# Model Evaluation part for EU_COnsumer_Qty
###########################################

# To evaluate the model, we are using last 6 months data

options(warn = -1)
global_pred_out <- predict(lm_fit , data.frame(Month = seq(43,48)))
options(warn = 1)

# comparing our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(global_pred_out,EU_Consumer_Qty[43:48])[5]
MAPE_class_dec #24.98

# The MAPE of our manually buit model is 24.98

##################################################
# Following is Auto ARIMA for EU_Consumer_Qty_ts
##################################################

autoarima <- auto.arima(EU_Consumer_Qty_ts)
autoarima #ARIMA(2,1,0) , AIC=529.8   AICc=530.44   BIC=534.94
tsdiag(autoarima)

plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

# Verify if the residual series is white noise

resi_auto_arima <- EU_Consumer_Qty_ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

# residual series is stationary

# Here we are evaluating the model built using autoa.arima
# We are using last 6 months of data

fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,EU_Consumer_Qty[43:48])[5]
MAPE_auto_arima # Wth Auto arima MAP value is coming as 30.13

# Plotting the predictions along with original values, to get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(EU_Consumer_Qty_ts, col = "black")
lines(auto_arima_pred, col = "red")

#############################################################################################################
# Since our manual model is better than arma model we will use our manual model to forecast the EU_Consumer Qty 
# for next 6 months: Mape for our manual model is 24.98 < 30.13 (Mape of model built via auto.arima)
#############################################################################################################

# Forecasting the quantity of next 6 months EU_Consumer Qty using our manual model.

options(warn = -1)
fcast_EU_Consumer_Qty_6_months <- predict(lm_fit , data.frame(Month = seq(49,54)))
options(warn = 1)

round(fcast_EU_Consumer_Qty_6_months) #758 707 640 583 573 645

##############################################################################################################
##############################################################################################################

#############################
#3. Using APAC_Consumer_Sales
#############################

# Using first 42 records of "APAC_COnsumer_Sales"  to build a training time series object

APAC_Consumer_Sales_ts <- create_timeseries(APAC_Consumer_Sales[1:42])

# Plotting the build timeseries object, built in previous step i.e APAC_Consumer_Sales_ts

plot(APAC_Consumer_Sales_ts , xlab = "Month" , ylab = "Total Sales Amount" , col = "blue" , 
     main = "Sales trend from Jan 2011 - Dec 2014 in APAC for Consumer segment")

# Smoothening the timeseries : i.e "APAC_Consumer_Sales_ts"

APAC_Consumer_Sales_ts_smoothen <- smoothen_timeseries(APAC_Consumer_Sales_ts)

# Plotting the smoothened times series over the existing plot of timeseries object

lines(APAC_Consumer_Sales_ts_smoothen, col="blue", lwd=2)

# Creating a dataframe from the smoothened time series object i.e APAC_Consumer_Sales_ts_smoothen

APAC_Consumer_Sales_ts_smoothen_df <- as.data.frame(cbind(seq(1:42), as.vector(APAC_Consumer_Sales_ts_smoothen)))

# Updating the column names to 'Month' and 'Sales'

colnames(APAC_Consumer_Sales_ts_smoothen_df) <- c('Month', 'Sales')

# Building a linear model that can be used to predict the values globally using the following formula

# We are trying to capture seasonality using sinusoidal function.

formula <- as.formula(Sales ~ sin(0.59*Month) * poly(Month,3) + cos(0.55*Month) * poly(Month,2) + Month)

# The built model is stored in lm_fit

lm_fit <- build_linear_model(APAC_Consumer_Sales_ts_smoothen_df , formula)

# Predicting the values of APAC_Consumer_Sales using the built model

global_pred <- predict(lm_fit, Month=APAC_Consumer_Sales_ts_smoothen_df$Month)
summary(global_pred)
lines(global_pred, col='red', lwd=2)

# build an arma series for local predictability

local_pred <- APAC_Consumer_Sales_ts - global_pred

# Plotting the local predictability pattern

plot(local_pred, col='red', type = "l")

#ACF PLOT

acf(local_pred)

# PACF plot

acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)  
tsdiag(armafit)

armafit #ARIMA(0,0,0) with zero mean , AIC=892.46   AICc=892.56   BIC=894.2

# Verify if the residual series is white noise
# using dickey fuller or kpss test

resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary") 

kpss.test(resi)

# The residual series is stationary

###############################################
# Model Evaluation part for APAC_Consumer_Sales
###############################################

# Predicting the values of APAC_Consumer_Sales for the last 6 months using our model

options(warn = -1)
global_pred_out <- predict(lm_fit , data.frame(Month = seq(43,48)))
options(warn = 1)

# Comparing our predicted values with the actual values, using MAPE

MAPE_class_dec <- accuracy(global_pred_out,APAC_Consumer_Sales[43:48])[5]
MAPE_class_dec #19.43

####################################################
# Following is Auto ARIMA for APAC_Consumer_Sales_ts
####################################################

autoarima <- auto.arima(APAC_Consumer_Sales_ts)
autoarima #ARIMA(0,1,1) , AIC=898.23   AICc=898.55   BIC=901.66
tsdiag(autoarima)


plot(autoarima$x, xlab = "Month" , ylab = "Total Sales Amount" , col = "blue" , 
     main = "Sales trend from Jan 2011 - Dec 2014 in APAC for Consumer segment")
lines(fitted(autoarima), col="red")

# Verifying if the residual series is white noise

resi_auto_arima <- APAC_Consumer_Sales_ts - fitted(autoarima)
adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

# The residual series is statioanary

# Evaluating the model built usng auto.arima using MAPE. Using the last 6 months of data

fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,APAC_Consumer_Sales[43:48])[5]
MAPE_auto_arima # Wth Auto arima MAPE value is coming as 27.68

# Plotting the predictions along with original values, to get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(APAC_Consumer_Sales_ts, col = "black")
lines(auto_arima_pred, col = "red")

################################################################################################################
# Since our manual model is better than arima model we will use our manual model forecast the APAC_Consumer_Sales 
# for next 6 months: Our Model Mape is 19.43 < 27.68 (Mape of auto arima model)
################################################################################################################

options(warn = -1)
fcast_APAC_Consumer_Sales_6_months <- predict(lm_fit , data.frame(Month = seq(49,54)))
options(warn = 1)

round(fcast_APAC_Consumer_Sales_6_months) #55741 43470 31071 23581 25149 37091

###############################################################################################################
###############################################################################################################

###########################
#4 .Using APAC_Consumer_Qty
###########################

# Using first 42 records of "APAC_COnsumer_Qty"  to build a training time series object

APAC_Consumer_Qty_ts <- create_timeseries(APAC_Consumer_Qty[1:42])

# Plotting the build timeseries object, built in previous step i.e APAC_Consumer_Qty_ts

plot(APAC_Consumer_Qty_ts , xlab = "Month" , ylab = "Total Qty" , col = "blue" , 
     main = "Qty trend from Jan 2011 - Dec 2014 in APAC for Consumer segment")

# Smoothening the timeseries : i.e "APAC_Consumer_Qty_ts"

APAC_Consumer_Qty_ts_smoothen <- smoothen_timeseries(APAC_Consumer_Qty_ts)

# Plotting the smoothened times series over the existing plot of timeseries object

lines(APAC_Consumer_Qty_ts_smoothen, col="blue", lwd=2)

# Creating a dataframe from the smoothened time series object i.e APAC_Consumer_Qty_ts_smoothen

APAC_Consumer_Qty_ts_smoothen_df <- as.data.frame(cbind(seq(1:42), as.vector(APAC_Consumer_Qty_ts_smoothen)))

# Updating the column names to 'Month' and 'Qty'

colnames(APAC_Consumer_Qty_ts_smoothen_df) <- c('Month', 'Qty')

# Building a linear model that can be used to predict the values globally using the following formula

# We are trying to capture seasonality using sinusoidal function.

formula <- as.formula(Qty ~ sin(0.59*Month) * poly(Month,3) + cos(0.55*Month) * poly(Month,3) + Month)

# The built model is stored in lm_fit

lm_fit <- build_linear_model(APAC_Consumer_Qty_ts_smoothen_df , formula)

# Predicting the values of APAC_Consumer_Qty using the built model

global_pred <- predict(lm_fit, Month=APAC_Consumer_Qty_ts_smoothen_df$Month)
summary(global_pred)
lines(global_pred, col='red', lwd=2)

# build an arma series for local predictability

local_pred <- APAC_Consumer_Qty_ts - global_pred

# Plotting the local predicatability pattern

plot(local_pred, col='red', type = "l")

#ACF PLOT

acf(local_pred)

# PACF plot

acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)  
tsdiag(armafit)

armafit # ARIMA(0,0,0) with zero mean , AIC=515.15   AICc=515.25   BIC=516.89

# Verify if the residual series is white noise
# Using Dickey fuller and kpss

resi <- local_pred-fitted(armafit)
adf.test(resi,alternative = "stationary") 

kpss.test(resi)

# The residual series is stationary

#############################################
# Model Evaluation part for APAC_Consumer_Qty
#############################################

# Evaluating the Model using MAPE, using the last 6 months of data

options(warn = -1)
global_pred_out <- predict(lm_fit , data.frame(Month = seq(43,48)))
options(warn = 1)

# Compare the predicted values with the actual values, using MAPE

MAPE_class_dec <- accuracy(global_pred_out,APAC_Consumer_Qty[43:48])[5]
MAPE_class_dec #22.81

##################################################
# Following is Auto ARIMA for APAC_Consumer_Qty_ts
##################################################

autoarima <- auto.arima(APAC_Consumer_Qty_ts)
autoarima #ARIMA(0,1,0) , AIC=534.14   AICc=534.24   BIC=535.85
tsdiag(autoarima)
plot(autoarima$x, col="black",)
lines(fitted(autoarima), col="red")

# Verify if the residual series is white noise

resi_auto_arima <- APAC_Consumer_Qty_ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

# The residual series is stationary

# Evaluate the model built using auto.arima using MAPE, using last 6 months of data

fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,APAC_Consumer_Qty[43:48])[5]
MAPE_auto_arima # Wth Auto arima MAP value is coming as 26.24

# Plotting the predictions along with original values, to get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(APAC_Consumer_Qty_ts, xlab = "Month" , ylab = "Total Qty" , col = "blue" , 
     main = "Qty trend from Jan 2011 - Dec 2014 in APAC for Consumer segment")
lines(auto_arima_pred, col = "red")

################################################################################################################
# Since our manual model is better than arima model we will use our manual model forecast the APAC_Consumer_Qty 
# for next 6 months: 22.81 < 26.24
################################################################################################################

options(warn = -1)
fcast_APAC_Consumer_Qty_6_months <- predict(lm_fit , data.frame(Month = seq(49,54)))
options(warn = 1)

round(fcast_APAC_Consumer_Qty_6_months) #508  417  470  697 1060 1446

##############################################################################################################
##############################################################################################################


###########################################
# Model Building II and Model Evaluation II
###########################################

# Here we have used Holt Winters Model for forecasting

############################################################################################################
#HOLT-WINTERS MODEL  EU_Consumer_Sales 
######################################################################################################

# Create train(input) & test(out) time series

ts_input_data_EU_consumer_sales<-ts(EU_Consumer_Sales[1:42],start=c(2011,1),frequency=12)
ts_out_data_EU_consumer_sales<-ts(EU_Consumer_Sales[43:48],start=c(2014,7),frequency=12)

autoplot(ts_input_data_EU_consumer_sales) # The graph shows that the trend is decreasing 
# & the amplitudes are waning very fast.This indicates a multiplicative model.

# Decompose the time series into Seasonal, trend & remainder
autoplot(decompose(ts_input_data_EU_consumer_sales, type="multiplicative"))

# 1. The Seasonality appears to be additive.
# 2. The trend appears to be multiplicative
# 3. Time series shows both trend and seasonality. Let us apply 
#    Holt-Winter's method (Holt's method is suitable if there is only trend and so,not selected)
# 3. ets stands for error,trend and seasonality
#     From decompose(), we see that the following 2 models can be tried
#     a) MAM & b) MAN 


Holt_Winter_EU_consumer_sales1 <- ets(ts_input_data_EU_consumer_sales, model = "MAM")
f1<-forecast(Holt_Winter_EU_consumer_sales1,h=12)
autoplot(f1)
summary(f1)
checkresiduals(f1) # All the residuals are fine
accuracy(f1, ts_out_data_EU_consumer_sales)

#p-value = 1.86e-05,MASE = 0.7484215, AICc =941.5956

Holt_Winter_EU_consumer_sales2 <- ets(ts_input_data_EU_consumer_sales, model = "MAN")
f2<-forecast(Holt_Winter_EU_consumer_sales2,h=12)
autoplot(f2)
summary(f2)
checkresiduals(f2) # All the residuals are fine
accuracy(f2, ts_out_data_EU_consumer_sales)

#p-value = 0.0001985,MASE = 2.0870114, AICc =949.5051

#CONCLUSION:   We will select the  earlier model "Holt_Winter_EU_consumer_sales1". The error in this model
#            is only 0.7484215%

##################################################################################################
#           #HOLT-winter-MODELS                           EU CONSUMER QUANTITY  
#################################################################################################

# Create train(input) & test(out) time series

ts_input_data_EU_consumer_quantity<-ts(EU_Consumer_Qty[1:42],start=c(2011,1),frequency=12)
ts_out_data_EU_consumer_quantity<-ts(EU_Consumer_Qty[43:48],start=c(2014,7),frequency=12)
autoplot(ts_input_data_EU_consumer_quantity)

# Decompose the time series into Seasonal, trend & remainder

autoplot(decompose(ts_input_data_EU_consumer_quantity, type="multiplicative"))

##########################################################
#                             HOLT-WINTERS MODEL MAM     #
##########################################################

Holt_Winter_EU_consumer_quantity3 <- ets(ts_input_data_EU_consumer_quantity, model = "MAM")
f3<-forecast(Holt_Winter_EU_consumer_quantity3,h=12)
autoplot(f3)
summary(f3)
checkresiduals(f3) # All the residuals are fine
accuracy(f3, ts_out_data_EU_consumer_quantity)

#p-value = 7.857e-09,MASE =1.4213443, AICc =556.2986

##########################################################
#                             HOLT-WINTERS MODEL MNA     #
##########################################################

Holt_Winter_EU_consumer_quantity4 <- ets(ts_input_data_EU_consumer_quantity, model = "MNA")
f4<-forecast(Holt_Winter_EU_consumer_quantity4,h=12)
autoplot(f4)
summary(f4)
checkresiduals(f4) # All the residuals are fine
accuracy(f4, ts_out_data_EU_consumer_quantity)

#p-value = 0.000397,MASE =1.8155494 , AICc =573.9426

#CONCLUSION: # We will select the   model "Holt_Winter_EU_consumer_quantity3". The error in this model
#            is only 1.4213443 %


#####################################################################################
#          #HOLT-winter-MODELS                          APAC CONSUMER SALES

#####################################################################################
# Create train(input) & test(out) time series

ts_input_data_APAC_consumer_sales<-ts(APAC_Consumer_Sales[1:42],start=c(2011,1),frequency=12)
ts_out_data_APAC_consumer_sales<-ts(APAC_Consumer_Sales[43:48],start=c(2014,7),frequency=12)

autoplot(ts_input_data_APAC_consumer_sales)

# Decompose the time series into Seasonal, trend & remainder

autoplot(decompose(ts_input_data_APAC_consumer_sales, type="multiplicative")) 

##########################################################
#                             HOLT-WINTERS MODEL MAN     #
##########################################################

Holt_Winter_APAC_consumer_sales5 <- ets(ts_input_data_APAC_consumer_sales, model = "MAN")
f5<-forecast(Holt_Winter_APAC_consumer_sales5,h=12)
autoplot(f5)
summary(f5)
checkresiduals(f5) # All the residuals are fine
accuracy(f5, ts_out_data_APAC_consumer_sales)

#p-value = 0.0007644,MASE =1.6311864, AICc =948.4598

############################################################
#                             HOLT-WINTERS MODEL  MMN      #
############################################################

Holt_Winter_APAC_consumer_sales6 <- ets(ts_input_data_APAC_consumer_sales, model = "MMN")
f6<-forecast(Holt_Winter_APAC_consumer_sales6,h=12)
autoplot(f6)
summary(f6)
checkresiduals(f6) # All the residuals are fine
accuracy(f6, ts_out_data_APAC_consumer_sales)

#p-value = 0.0009109,MASE =1.813542, AICc =948.7509

#CONCLUSION: #  We will select the   model "Holt_Winter_APAC_consumer_sales5". The error in this model
#            is only 1.6311864%


#####################################################################################
#        #HOLT-winter-MODELS                       APAC CONSUMER Quantity

#####################################################################################

# Create train(input) & test(out) time series

ts_input_data_APAC_consumer_quantity<-ts(APAC_Consumer_Qty[1:42],start=c(2011,1),frequency=12)
ts_out_data_APAC_consumer_quantity<-ts(APAC_Consumer_Qty[43:48],start=c(2014,7),frequency=12)

autoplot(ts_input_data_APAC_consumer_quantity)

# Decompose the time series into Seasonal, trend & remainder

autoplot(decompose(ts_input_data_APAC_consumer_quantity, type="multiplicative")) 

##########################################################
#                             HOLT-WINTERS MODEL MAM     #
##########################################################

Holt_Winter_APAC_consumer_quantity7 <- ets(ts_input_data_APAC_consumer_quantity, model = "MAM")
f7<-forecast(Holt_Winter_APAC_consumer_quantity7,h=12)
autoplot(f7)
summary(f7)
checkresiduals(f7) # All the residuals are fine
accuracy(f7, ts_out_data_APAC_consumer_quantity)

#p-value =  3.582e-05,MASE =0.7731437, AICc =568.2969

############################################################
#                             HOLT-WINTERS MODEL  MNM      #
############################################################                                            

Holt_Winter_APAC_consumer_quantity8 <- ets(ts_input_data_APAC_consumer_quantity, model = "MNM")
f8<-forecast(Holt_Winter_APAC_consumer_quantity8,h=12)
autoplot(f8)
summary(f8)
checkresiduals(f8) # All the residuals are fine
accuracy(f8, ts_out_data_APAC_consumer_quantity)


#p-value =  0.0001362,MASE =0.8036276, AICc =568.9883

#CONCLUSION: # We will select the model "Holt_Winter_APAC_consumer_quantity7". The error in this model
#            is only 0.7731437%

####################################END############################################################



