
#######################################################################
#   Case study - Assignment - Linear Regression - Geely Auto          #
#   Name: Manohar Shanmugasundaram                                    #
#   Roll ID: DDA1730068                                               #
#######################################################################

# import the required libraries
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)
library(MASS)
library(car)
library(readr)    
library(PerformanceAnalytics)

# Read the file into the dataframe
carprice <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = TRUE)

View(carprice)
str(carprice)

#####################################
##    Step 1 - Data Cleaning       ##
#####################################

# 1. Check whether there are any 'NA' values in the data set
sum(is.na(carprice))

# Result: 0 - No 'NA' values in the dataset

# 2. Check for duplicate or extra rows and remove (if there any)
nrow(unique(carprice))

# Result: 205 - No duplicates rows in the dataset

##############################################################
#         Outlier Analysis (independent variables)          ##
##############################################################

# 3. Check for outliers in all continous independent variables

# a. peakrpm 
quantile(carprice$peakrpm,seq(0,1,0.01))
# Result - There is an outlier from 99% to 100%, hence the outlier 100% will be removed with the value 6000 (99%)
carprice$peakrpm[carprice$peakrpm > 6000] <- 6000

# b. wheelbase 
quantile(carprice$wheelbase,seq(0,1,0.01))
# Result - There is an outlier from 99% to 100%, hence the outlier 100% will be removed with the value 115.544 (99%)
carprice$wheelbase[carprice$wheelbase > 115.544] <- 115.544

# c. carlength 
quantile(carprice$carlength,seq(0,1,0.01))
# Result - There is an outlier from 99% to 100%, hence the outlier 100% will be removed with the value 202.480 (99%)
carprice$carlength[carprice$carlength > 202.480] <- 202.480

# d. carwidth 
quantile(carprice$carwidth,seq(0,1,0.01))
# Result - no outliers in this field

# e. carheight 
quantile(carprice$carheight,seq(0,1,0.01))
# Result - no outliers in this field

# f. curbweight 
quantile(carprice$curbweight,seq(0,1,0.01))
# Result - There is an outlier from 0% to 1%, hence the outlier 0% will be removed with the value 1819.72 (1%)
carprice$curbweight[carprice$curbweight <= 1819.72] <- 1819.72

# f. enginesize 
quantile(carprice$enginesize,seq(0,1,0.01))
# Result - There is an outlier from 98% to 100%, hence the outlier from 98% will be removed with the value 256.08 (98%)
carprice$enginesize[carprice$enginesize > 256.08] <- 256.08

# g. boreratio 
quantile(carprice$boreratio,seq(0,1,0.01))
# Result - There is an outlier from 0% to 1%, hence the outlier 0% will be removed with the value 2.9100 (1%)
carprice$boreratio[carprice$boreratio <= 2.9100] <- 2.9100

# h. stroke 
quantile(carprice$stroke,seq(0,1,0.01))
# Result - There is an outlier from 0% to 2%, hence the outlier below 2% will be removed with the value 2.6400 (2%)
carprice$stroke[carprice$stroke <= 2.6400] <- 2.6400

# i. compressionratio 
quantile(carprice$compressionratio,seq(0,1,0.01))
# Result - no outliers in this field

# j. horsepower 
quantile(carprice$horsepower,seq(0,1,0.01))
# Result - There is an outlier from 99% to 100%, hence the outlier 100% will be removed with the value 207.00 (99%)
carprice$horsepower[carprice$horsepower > 207.00] <- 207.00

# k. citympg 
quantile(carprice$citympg,seq(0,1,0.01))
# Result - There is an outlier from 98% to 100%, hence the outlier 99% & 100% will be removed with the value 38.00 (98%)
carprice$citympg[carprice$citympg > 38.00] <- 38.00

# l. highwaympg 
quantile(carprice$highwaympg,seq(0,1,0.01))
# Result - There is an outlier from 99% to 100%, hence the outlier 100% will be removed with the value 49.88 (99%)
carprice$highwaympg[carprice$highwaympg > 49.88] <- 49.88


###########################################
##          Derived Metrics              ##
###########################################

##########  Split the column  ##################
# 4. Split the CarName and get only the car manufacture name and ignore the model name.
carprice$carbrand <- as.factor(str_split(carprice$CarName, " ", n = 2,simplify = TRUE)[,1])

# 5. Remove the not required fields like CarID, CarName
carprice <- carprice[-1] # directly removed the column #1 - CarID
carprice <- carprice[-2] # directly removed the column #1 - CarName

# 6. Fixing invalid values in the Car brand name, following are the errors in the values and will be fixed as follows
#  Wrong Name        Correct Name
#  ----------       ------------  
#  maxda            mazda
#  porcshce         porsche
#  toyouta          toyota
#  vokswagen        volkswagen
#  vw               volkswagen (identified using external source)

carprice$carbrand <- gsub("maxda", "mazda", carprice$carbrand)
carprice$carbrand <- gsub("porcshce", "porsche", carprice$carbrand)
carprice$carbrand <- gsub("toyouta", "toyota", carprice$carbrand)
carprice$carbrand <- gsub("vokswagen", "volkswagen", carprice$carbrand)
carprice$carbrand <- gsub("vw", "volkswagen", carprice$carbrand)

# 7. Standardise values - Convert the cases for manufacture to match with all other records.
# Conver 'Nissan' to 'nissan'
carprice$carbrand <- gsub("Nissan", "nissan", carprice$carbrand)

carprice$carbrand <- as.factor(carprice$carbrand)

# 8. Create a derived variable for price for the EDA analysis
#     Price                     Annual income
#     -------------             --------------
#     <=10000 thousand          <= 10000
#     10000 to 20000 thousand   > 10000 and <= 20000
#     10000 to 20000 thousand   > 20000 and <= 30000
#     10000 to 20000 thousand   > 30000 and <= 40000
#     > 40000 thousand          > 40000

## Note, this variable will be removed after the EDA analysis before doing the models

carprice$price_group <- cut((carprice$price), 
                            breaks=c(0,10000,20000,30000,40000,50000), 
                            labels=c("<=10 thousand","10 to 20 thousand","20 to 30 thousand","30 to 40 thousand",
                                     ">40 thousand"),include.lowest=T, na.rm = TRUE)

summary(carprice)

###################################################
##  Step 2 - Univariate and Bivariate Analysis   ##
###################################################

############# Univariate Analysis on CATEGORICAL VARIABLES ##########

# 1. Analyse the new Car brand variable
ggplot(data=carprice,aes(carbrand)) +
  geom_bar(color=I('black'),fill=I('#56B4E9')) +
  ggtitle("Bar Plot for car brand") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Insight from plot - Toyota have higher volume of cars compared to all the other car brands

# 2. Analyse the fueltype variable
ggplot(data=carprice,aes(fueltype)) +
  geom_bar(color=I('black'),fill=I('#56B4E9')) +
  ggtitle("Bar Plot for Fuel Type") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=3)

# Insight from plot - There are more cars with the fuel type 'gas' than 'diesel'

# 3. Analyse the aspiration variable
ggplot(data=carprice,aes(aspiration)) +
  geom_bar(color=I('black'),fill=I('#56B4E9')) +
  ggtitle("Bar Plot for aspiration") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=3)

# Insight from plot - There are more cars with the aspiration 'std' than 'turbo'

# 4. Analyse the carbody variable
ggplot(data=carprice,aes(carbody)) +
  geom_bar(color=I('black'),fill=I('#56B4E9')) +
  ggtitle("Bar Plot for carbody") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=3)

# Insight from plot - 'sedan' and 'hatchback' seems to have more cars

# 5. Analyse the enginelocation variable
ggplot(data=carprice,aes(enginelocation)) +
  geom_bar(color=I('black'),fill=I('#56B4E9')) +
  ggtitle("Bar Plot for engine location") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=3)

# Insight from plot - 'front' engine cars seems to be nearly 98% of cars compared to the 'rear' engine ones

# 6. Analyse the drivewheel variable
ggplot(data=carprice,aes(drivewheel)) +
  geom_bar(color=I('black'),fill=I('#56B4E9')) +
  ggtitle("Bar Plot for drivewheel") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=3)

# Insight from plot - 'fwd' type has more cars compared to the other types 'rwd' and '4wd'

# 7. Analyse the enginetype variable
ggplot(data=carprice,aes(enginetype)) +
  geom_bar(color=I('black'),fill=I('#56B4E9')) +
  ggtitle("Bar Plot for enginetype") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=3)

# Insight from plot - 'ohc' engine type has more cars compared to the other types

# 8. Analyse the cylindernumber variable
ggplot(data=carprice,aes(cylindernumber)) +
  geom_bar(color=I('black'),fill=I('#56B4E9')) +
  ggtitle("Bar Plot for cylinder number") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=3)

# Insight from plot - 'four' cylinder cars are very high compared to the other types

# 8. Analyse the cylindernumber variable
ggplot(data=carprice,aes(cylindernumber)) +
  geom_bar(color=I('black'),fill=I('#56B4E9')) +
  ggtitle("Bar Plot for cylinder number") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=3)

# Insight from plot - 'four' cylinder cars are very high compared to the other types

# 9. Analyse the fuelsystem variable
ggplot(data=carprice,aes(fuelsystem)) +
  geom_bar(color=I('black'),fill=I('#56B4E9')) +
  ggtitle("Bar Plot for fuel system") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=3)

# Insight from plot - 'mpfi' and '2bbl' cars are more compared to the other types

##########   BIVARIATE ANALYSIS - CATEGORICAL & CONTINUOUS VARIABLES ################

# 1. BAR plot for price group and car
ggplot(data = carprice, aes(x=carbrand, fill=as.factor(price_group))) + geom_bar(position = 'dodge') +
  labs(x = "car brand") + labs(y = "price group") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name="Price Group")

# Insight from plot - This graph provided a good insight on the price ranges for each brand, example
# Toyoto, nissan, honda, mitsubushi have car price ranges only between 0 to 20 thousand
# buick, porsche and jaguar have care price ranges only between 30 to 46 thousand
# bmw, volvo and audi have mixed set of cars with price ranges between 10 and 46 thousand

# 2. BAR plot for price group and car body
ggplot(data = carprice, aes(x=carbody, fill=as.factor(price_group))) + geom_bar(position = 'dodge') +
  labs(x = "car body") + labs(y = "price group") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name="Price Group")

# Insight from plot - 'sedan' and 'hatchback' seems to be most popular car body type covering most price groups

# 3. BAR plot for price group and enginetype
ggplot(data = carprice, aes(x=enginetype, fill=as.factor(price_group))) + geom_bar(position = 'dodge') +
  labs(x = "engine type") + labs(y = "price group") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name="Price Group")

# Insight from plot - Engine type 'ohc' seems to be most popular car body type covering most price groups

# Remove the categorical group variable created before doing the model
carprice <- carprice[-26]

###########################################################
##  Step 3 - Linear Regression - Predictive Analysis     ##
###########################################################

#########################################################################################
##  1. Covert the categorical variables into numeric variables for linear regression   ##
#########################################################################################

# a. Move the Aspiration - 'std' as 1 and 'turbo' as 0 and store in the same variable
levels(carprice$aspiration)<-c(1,0)
carprice$aspiration <- as.numeric(levels(carprice$aspiration))[carprice$aspiration]

# b. Move the fueltype - 'gas' as 1 and 'diesel' as 0 and store in the same variable
levels(carprice$fueltype)<-c(0,1)
carprice$fueltype <- as.numeric(levels(carprice$fueltype))[carprice$fueltype]

# c. Move the doornumber - 'four' as 0 and 'two' as 1 and store in the same variable
levels(carprice$doornumber)<-c(0,1)
carprice$doornumber <- as.numeric(levels(carprice$doornumber))[carprice$doornumber]

# d. Move the drivewheel - '4wd' as 0, 'fwd' as 1 and 'rwd' as 2 and store in the same variable
levels(carprice$drivewheel)<-c(0,1,2)
carprice$drivewheel <- as.numeric(levels(carprice$drivewheel))[carprice$drivewheel]

# e. Move the drivewheel - 'front' as 0 and 'rear' as 2 and store in the same variable
levels(carprice$enginelocation)<-c(0,1)
carprice$enginelocation <- as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]

############################################################
##              DUMMY VARIABLE CREATION.                  ##
## Creation of dummy variables to use in regression model ##
############################################################

# a. Convert the car brand name into numeric using the dummy variables
dummy_car  <- data.frame(model.matrix( ~carbrand, data = carprice))

# remove the unwanted column, generated as part of the previous command
dummy_car <- dummy_car[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "CarName" column
carprice<- cbind(carprice[,-25], dummy_car)

# b. Convert the carbody into numeric using the dummy variables
dummy_carbody <- data.frame(model.matrix( ~carbody, data = carprice))

# remove the unwanted column, generated as part of the previous command
dummy_carbody <- dummy_carbody[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "CarName" column
carprice<- cbind(carprice[,-5], dummy_carbody)

# c. Convert the enginetype into numeric using the dummy variables
dummy_enginetype <- data.frame(model.matrix( ~enginetype, data = carprice))

# remove the unwanted column, generated as part of the previous command
dummy_enginetype <- dummy_enginetype[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "CarName" column
carprice<- cbind(carprice[,-12], dummy_enginetype)

# d. Convert the cylindernumber into numeric using the dummy variables
dummy_cylindernumber <- data.frame(model.matrix( ~cylindernumber, data = carprice))

# remove the unwanted column, generated as part of the previous command
dummy_cylindernumber <- dummy_cylindernumber[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "CarName" column
carprice<- cbind(carprice[,-12], dummy_cylindernumber)

# d. Convert the fuelsystem into numeric using the dummy variables
dummy_fuelsystem <- data.frame(model.matrix( ~fuelsystem, data = carprice))

# remove the unwanted column, generated as part of the previous command
dummy_fuelsystem <- dummy_fuelsystem[,-1]

# Combine the dummy variables to the main data set, after removing the original categorical "CarName" column
carprice<- cbind(carprice[,-13], dummy_fuelsystem)

str(carprice)

###########################
# Correlation Analysis    #
###########################

# Create a correlation matrix with all the variables in the data set
corr_df<-as.data.frame(cor(carprice)) 
View(corr_df)

write.csv(corr_df,"carprice_cormatrix.csv")

######################################
##    Step 4 - Model Creation       ##
######################################

# separate training and testing data
set.seed(100)

# Get the training and test data set values
train = sample(1:nrow(carprice), 0.7*nrow(carprice))
train_carprice = carprice[train,]
test_carprice = carprice[-train,]

#####################################
##      Model 1
## Consider all the variables
####################################

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train_carprice)

summary(model_1)
# Multiple R-squared:  0.9785,	Adjusted R-squared:  0.9654 

# use stepAIC to eliminate the variables
step <- stepAIC(model_1, direction="both")

step

# Removed the followong variables after the stepAIC

# fueltype, fuelsystemidi, boreratio, peakrpm, carbrandporsche, enginetypedohcv, fuelsystemmfi, enginetypeohcv,
# carbodyhatchback, carlength, horsepower, fuelsystem2bbl, symboling, fuelsystemmpfi, highwaympg, carbodysedan,
# doornumber, carbodyhardtop, enginetyperotor, cylindernumbertwo, fuelsystem4bbl, citympg, carheight,
# fuelsystemspdi, carbrandaudi

########################################
##      Model 2
## Consider the variables after stepAIC
########################################

model_2 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                wheelbase + carwidth + curbweight + enginesize + stroke + 
                compressionratio + carbrandbmw + carbrandbuick + carbrandchevrolet + 
                carbranddodge + carbrandhonda + carbrandisuzu + carbrandjaguar + 
                carbrandmazda + carbrandmercury + carbrandmitsubishi + carbrandnissan + 
                carbrandpeugeot + carbrandplymouth + carbrandrenault + carbrandsaab + 
                carbrandsubaru + carbrandtoyota + carbrandvolkswagen + carbrandvolvo + 
                carbodywagon + enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train_carprice)

summary(model_2)
# Multiple R-squared:  0.9766,	Adjusted R-squared:  0.9695 

# Check the VIF
vif(model_2)

# Result - VIF for many variables in above 5. As per the analysis removing the compressionratio, since it has
# less significance based on the p-value.

###########################################
##  Model 3 
##  Remove the variable - compressionratio
##########################################

model_3 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                wheelbase + carwidth + curbweight + enginesize + stroke + 
                carbrandbmw + carbrandbuick + carbrandchevrolet + 
                carbranddodge + carbrandhonda + carbrandisuzu + carbrandjaguar + 
                carbrandmazda + carbrandmercury + carbrandmitsubishi + carbrandnissan + 
                carbrandpeugeot + carbrandplymouth + carbrandrenault + carbrandsaab + 
                carbrandsubaru + carbrandtoyota + carbrandvolkswagen + carbrandvolvo + 
                carbodywagon + enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train_carprice)
summary(model_3)
# Multiple R-squared:  0.9762,	Adjusted R-squared:  0.9693 

# check the VIF 
vif(model_3)
# There are many VIFs above 5, so will remove the value based on the p-value

# Remove carbodywagon based on insignificance. It has the highest p value of 0.165709 among the remaining variables

#######################################
##  Model 4 
##  Remove the variable - carbodywagon
#######################################

model_4 <- lm(formula = price ~ aspiration + drivewheel + enginelocation + 
                wheelbase + carwidth + curbweight + enginesize + stroke + 
                carbrandbmw + carbrandbuick + carbrandchevrolet + 
                carbranddodge + carbrandhonda + carbrandisuzu + carbrandjaguar + 
                carbrandmazda + carbrandmercury + carbrandmitsubishi + carbrandnissan + 
                carbrandpeugeot + carbrandplymouth + carbrandrenault + carbrandsaab + 
                carbrandsubaru + carbrandtoyota + carbrandvolkswagen + carbrandvolvo + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train_carprice)
summary(model_4)
# Multiple R-squared:  0.9758,	Adjusted R-squared:  0.9691

# Check the VIF
vif(model_4)
# There are many VIFs above 5, so will remove the value based on the p-value. 
# Remove drivewheel based on insignificance. 

#######################################
##  Model 5
##  Remove the variable - drivewheel
#######################################

model_5 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + enginesize + stroke + 
                carbrandbmw + carbrandbuick + carbrandchevrolet + 
                carbranddodge + carbrandhonda + carbrandisuzu + carbrandjaguar + 
                carbrandmazda + carbrandmercury + carbrandmitsubishi + carbrandnissan + 
                carbrandpeugeot + carbrandplymouth + carbrandrenault + carbrandsaab + 
                carbrandsubaru + carbrandtoyota + carbrandvolkswagen + carbrandvolvo + 
                enginetypeohc + cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train_carprice)
summary(model_5)
# Multiple R-squared:  0.975,	Adjusted R-squared:  0.9683

# Calculate the vif for model_5
vif(model_5)
# There are many VIFs above 5, so will remove the value based on the p-value
# Remove enginetypeohc based on insignificance.

#######################################
##  Model 6
##  Remove the variable - enginetypeohc
#######################################

model_6 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + enginesize + stroke + 
                carbrandbmw + carbrandbuick + carbrandchevrolet + 
                carbranddodge + carbrandhonda + carbrandisuzu + carbrandjaguar + 
                carbrandmazda + carbrandmercury + carbrandmitsubishi + carbrandnissan + 
                carbrandpeugeot + carbrandplymouth + carbrandrenault + carbrandsaab + 
                carbrandsubaru + carbrandtoyota + carbrandvolkswagen + carbrandvolvo + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train_carprice)
summary(model_6)
# Multiple R-squared:  0.9745,	Adjusted R-squared:  0.9679

# Check the VIF
vif(model_6)
# There are many VIFs above 5, so will remove the value based on the p-value
# Remove carbrandsaab based on insignificance.

######################################
##  Model 7
##  Remove the variable - carbrandsaab
#######################################

model_7 <-  lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + carwidth + curbweight + enginesize + stroke + 
                 carbrandbmw + carbrandbuick + carbrandchevrolet + 
                 carbranddodge + carbrandhonda + carbrandisuzu + carbrandjaguar + 
                 carbrandmazda + carbrandmercury + carbrandmitsubishi + carbrandnissan + 
                 carbrandpeugeot + carbrandplymouth + carbrandrenault + 
                 carbrandsubaru + carbrandtoyota + carbrandvolkswagen + carbrandvolvo + 
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix, data = train_carprice)
summary(model_7)
# Multiple R-squared:  0.9735,	Adjusted R-squared:  0.967

# Check the VIF
vif(model_7)
# There are many VIFs above 5, so will remove the value based on the p-value
# Remove carbrandhonda based on the insignificance. It has the highest p value of 0.132761 
# and zero '*' among the remaining variables

######################################
##  Model 8
##  Remove the variable - carbrandhonda
#######################################

model_8 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + enginesize + stroke + 
                carbrandbmw + carbrandbuick + carbrandchevrolet + 
                carbranddodge + carbrandisuzu + carbrandjaguar + 
                carbrandmazda + carbrandmercury + carbrandmitsubishi + carbrandnissan + 
                carbrandpeugeot + carbrandplymouth + carbrandrenault + 
                carbrandsubaru + carbrandtoyota + carbrandvolkswagen + carbrandvolvo + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train_carprice)
summary(model_8)
# Multiple R-squared:  0.9729,	Adjusted R-squared:  0.9666 

# Check the VIF
vif(model_8)
# There are many VIFs above 5, so will remove the value based on the p-value
# Remove carbrandrenault. It has the highest p value of 0.112104 and zero '*' among the remaining variables

######################################
##  Model 9
##  Remove the variable - carbrandrenault
#######################################

model_9 <- lm(formula = price ~ aspiration + enginelocation + 
                wheelbase + carwidth + curbweight + enginesize + stroke + 
                carbrandbmw + carbrandbuick + carbrandchevrolet + 
                carbranddodge + carbrandisuzu + carbrandjaguar + 
                carbrandmazda + carbrandmercury + carbrandmitsubishi + carbrandnissan + 
                carbrandpeugeot + carbrandplymouth + carbrandrenault + 
                carbrandsubaru + carbrandtoyota + carbrandvolkswagen + carbrandvolvo + 
                cylindernumberfive + cylindernumberfour + 
                cylindernumbersix, data = train_carprice)
summary(model_9)
# Multiple R-squared:  0.9729,	Adjusted R-squared:  0.9666

# Check the VIF
vif(model_9)
# There are few VIFs above 5, so will remove the value based on the p-value
# Remove carbrandrenault. It has the highest p value of 0.112104 and zero '*' among the remaining variables

#########################################
##  Model 10
##  Remove the variable - carbrandrenault
#########################################

model_10 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + carwidth + curbweight + enginesize + stroke + 
                 carbrandbmw + carbrandbuick + carbrandchevrolet + 
                 carbranddodge + carbrandisuzu + carbrandjaguar + 
                 carbrandmazda + carbrandmercury + carbrandmitsubishi + carbrandnissan + 
                 carbrandpeugeot + carbrandplymouth + 
                 carbrandsubaru + carbrandtoyota + carbrandvolkswagen + carbrandvolvo + 
                 cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix, data = train_carprice)
summary(model_10)
# Multiple R-squared:  0.9723,	Adjusted R-squared:  0.9661

# Check the VIF
vif(model_10)
# There are few VIFs above 5, so will remove the value based on the p-value
# Remove carbrandisuzu. It has the highest p value of 0.083456 and zero '*' among the remaining variables

#######################################
##  Model 11
##  Remove the variable - carbrandisuzu
#######################################

model_11 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + carwidth + curbweight + enginesize + stroke + 
                 carbrandbmw + carbrandbuick + carbrandchevrolet + 
                 carbranddodge + carbrandjaguar + carbrandmazda + carbrandmercury + carbrandmitsubishi + 
                 carbrandnissan + carbrandpeugeot + carbrandplymouth + carbrandsubaru + carbrandtoyota + 
                 carbrandvolkswagen + carbrandvolvo + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix, data = train_carprice)
summary(model_11)
# Multiple R-squared:  0.9716,	Adjusted R-squared:  0.9655 

# Check the VIF
vif(model_11)
# Remove carbrandvolvo, it has the highest p value of 0.051538 and zero '*' among the remaining variables

#######################################
##  Model 12
##  Remove the variable - carbrandvolvo
#######################################

model_12 <- lm(formula = price ~ aspiration + enginelocation + 
                 wheelbase + carwidth + curbweight + enginesize + stroke + 
                 carbrandbmw + carbrandbuick + carbrandchevrolet + 
                 carbranddodge + carbrandjaguar + carbrandmazda + carbrandmercury + carbrandmitsubishi + 
                 carbrandnissan + carbrandpeugeot + carbrandplymouth + carbrandsubaru + carbrandtoyota + 
                 carbrandvolkswagen + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix, data = train_carprice)
summary(model_12)
# Multiple R-squared:  0.9707,	Adjusted R-squared:  0.9647

# Check the VIF
vif(model_12)
# There are few VIFs above 5, so will remove the value based on the p-value
# Remove wheelbase, it has the highest p value of 0.167193 and zero '*' among the remaining variables

#######################################
##  Model 13
##  Remove the variable - wheelbase
#######################################

model_13 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + 
                 carbrandbmw + carbrandbuick + carbrandchevrolet + 
                 carbranddodge + carbrandjaguar + carbrandmazda + carbrandmercury + carbrandmitsubishi + 
                 carbrandnissan + carbrandpeugeot + carbrandplymouth + carbrandsubaru + carbrandtoyota + 
                 carbrandvolkswagen + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix, data = train_carprice)
summary(model_13)
# Multiple R-squared:  0.9702,	Adjusted R-squared:  0.9644

# Check the VIF
vif(model_13)
# There are few VIFs above 5, so will remove the value based on the p-value
# Remove carbrandmercury, it has the highest p value of 0.08156 and zero '*' among the remaining variables

#########################################
##  Model 14
##  Remove the variable - carbrandmercury
#########################################

model_14 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + 
                 carbrandbmw + carbrandbuick + carbrandchevrolet + 
                 carbranddodge + carbrandjaguar + carbrandmazda + carbrandmitsubishi + 
                 carbrandnissan + carbrandpeugeot + carbrandplymouth + carbrandsubaru + carbrandtoyota + 
                 carbrandvolkswagen + cylindernumberfive + cylindernumberfour + 
                 cylindernumbersix, data = train_carprice)
summary(model_14)
# Multiple R-squared:  0.9694,	Adjusted R-squared:  0.9638

# Check the VIF
vif(model_14)
# There are few VIFs above 5, so will remove the value based on the p-value
# Remove carbrandvolkswagen, it has the highest p value of 0.089749 and zero '*' among the remaining variables

##############################################
##  Model 15
##  Remove the variable -  carbrandvolkswagen
#############################################

model_15 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + 
                 carbrandbmw + carbrandbuick + carbrandchevrolet + 
                 carbranddodge + carbrandjaguar + carbrandmazda + carbrandmitsubishi + 
                 carbrandnissan + carbrandpeugeot + carbrandplymouth + carbrandsubaru + carbrandtoyota + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix, data = train_carprice)
summary(model_15)
# Multiple R-squared:  0.9687,	Adjusted R-squared:  0.9632

# Check the VIF
vif(model_15)
# Remove carbrandnissan, it has the highest p value of 0.048005 and one '*' and it is the insignificant variable
# among the remaining variables.

#########################################
##  Model 16
##  Remove the variable -  carbrandnissan
#########################################
model_16 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + 
                 carbrandbmw + carbrandbuick + carbrandchevrolet + 
                 carbranddodge + carbrandjaguar + carbrandmazda + carbrandmitsubishi + 
                 carbrandpeugeot + carbrandplymouth + carbrandsubaru + carbrandtoyota + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix, data = train_carprice)
summary(model_16)
# Multiple R-squared:  0.9676,	Adjusted R-squared:  0.9623

# Check the VIF
vif(model_16)
# Remove carbranddodge, it has the highest p value of 0.056314 and zero '*' and it is the insignificant variable
# among the remaining variables.

########################################
##  Model 17
##  Remove the variable -  carbranddodge
########################################

model_17 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + 
                 carbrandbmw + carbrandbuick + carbrandchevrolet + 
                 carbrandjaguar + carbrandmazda + carbrandmitsubishi + 
                 carbrandpeugeot + carbrandplymouth + carbrandsubaru + carbrandtoyota + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix, data = train_carprice)
summary(model_17)
# Multiple R-squared:  0.9667,	Adjusted R-squared:  0.9615 

# Check the VIF
vif(model_17)
# Remove carbrandplymouth, since it has the highest p value of 0.080368 and zero '*' among the remaining variables

###########################################
##  Model 18
##  Remove the variable -  carbrandplymouth
###########################################

model_18 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + 
                 carbrandbmw + carbrandbuick + carbrandchevrolet + 
                 carbrandjaguar + carbrandmazda + carbrandmitsubishi + 
                 carbrandpeugeot + carbrandsubaru + carbrandtoyota + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix, data = train_carprice)
summary(model_18)
# Multiple R-squared:  0.9658,	Adjusted R-squared:  0.9608 

# Check the VIF
vif(model_18)
# Remove carbrandchevrolet, it has the highest p value of 0.082206 and zero '*' among the remaining variables

############################################
##  Model 19
##  Remove the variable -  carbrandchevrolet
############################################

model_19 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + 
                 carbrandbmw + carbrandbuick + carbrandjaguar + carbrandmazda + carbrandmitsubishi + 
                 carbrandpeugeot + carbrandsubaru + carbrandtoyota + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix, data = train_carprice)
summary(model_19)
# Multiple R-squared:  0.965,	Adjusted R-squared:  0.9602

# Check the VIF
vif(model_19)
# Remove carbrandmazda, it has the highest p value of 0.011744 and one '*' among the remaining variables

#########################################
##  Model 20
##  Remove the variable -  carbrandmazda
#########################################

model_20 <- lm(formula = price ~ aspiration + enginelocation + 
                 carwidth + curbweight + enginesize + stroke + 
                 carbrandbmw + carbrandbuick + carbrandjaguar + carbrandmitsubishi + 
                 carbrandpeugeot + carbrandsubaru + carbrandtoyota + 
                 cylindernumberfive + cylindernumberfour + cylindernumbersix, data = train_carprice)
summary(model_20)
# Multiple R-squared:  0.9631,	Adjusted R-squared:  0.9584 

# Check the VIF
vif(model_20)
# Remove carbrandmitsubishi, it has the highest p value of 0.012980 and one '*' among the remaining variables

##############################################
##  Model 21
##  Remove the variable -  carbrandmitsubishi
#############################################

model_21 <- lm(formula = price ~ aspiration + enginelocation + carwidth + curbweight + enginesize + stroke + 
                carbrandbmw + carbrandbuick + carbrandjaguar + carbrandpeugeot + carbrandsubaru + 
                carbrandtoyota + cylindernumberfive + cylindernumberfour + cylindernumbersix, 
                data = train_carprice)
summary(model_21)
# Multiple R-squared:  0.9613,	Adjusted R-squared:  0.9567

# Check the VIF
vif(model_21)
# Remove aspiration. Since it has the highest p value of 0.002746 and two '*' among the remaining variables

#######################################
##  Model 22
##  Remove the variable -  aspiration
#######################################

model_22 <<- lm(formula = price ~ enginelocation + carwidth + curbweight + enginesize + stroke + 
                  carbrandbmw + carbrandbuick + carbrandjaguar + carbrandpeugeot + carbrandsubaru + 
                  carbrandtoyota + cylindernumberfive + cylindernumberfour + cylindernumbersix, 
                data = train_carprice)
summary(model_22)
# Multiple R-squared:  0.9584,	Adjusted R-squared:  0.9539 

# Check the VIF
vif(model_22)

# Note: All the have high significance, i.e, three '***' after the model 22. Let's predict and see the model.

#################################################################
# Coefficients:
#                    Estimate Std. Error     t value Pr(>|t|)    
#  (Intercept)        -4.171e+04  9.978e+03  -4.180 5.35e-05 ***
#  enginelocation      1.634e+04  1.390e+03  11.753  < 2e-16 ***
#  carwidth            8.072e+02  1.648e+02   4.899 2.85e-06 ***
#  curbweight          4.614e+00  8.479e-01   5.442 2.59e-07 ***
#  enginesize          6.015e+01  1.188e+01   5.061 1.41e-06 ***
#  stroke             -4.225e+03  7.553e+02  -5.594 1.29e-07 ***
#  carbrandbmw         7.545e+03  1.112e+03   6.784 3.90e-10 ***
#  carbrandbuick       5.071e+03  1.103e+03   4.597 1.01e-05 ***
#  carbrandjaguar      7.649e+03  1.608e+03   4.757 5.21e-06 ***
#  carbrandpeugeot    -3.722e+03  8.990e+02  -4.140 6.25e-05 ***
#  carbrandsubaru     -4.013e+03  9.076e+02  -4.422 2.07e-05 ***
#  carbrandtoyota     -1.464e+03  4.117e+02  -3.556 0.000529 ***
#  cylindernumberfive -3.684e+03  9.034e+02  -4.078 7.94e-05 ***
#  cylindernumberfour -4.189e+03  7.562e+02  -5.539 1.65e-07 ***
#  cylindernumbersix  -4.767e+03  1.022e+03  -4.663 7.74e-06 ***

###########################################
###########################################
#       5.Model Evaluation                #
###########################################
###########################################

# Predict the house prices in the testing dataset
Predict_1 <- predict(model_22,test_carprice)
test_carprice$test_price <- Predict_1

# Accuracy of the predictions
# Calculate correlation
r <- cor(test_carprice$price,test_carprice$test_price)
# calculate R squared by squaring correlation
rsquared <- cor(test_carprice$price,test_carprice$test_price)^2

# check R-squared
rsquared
# 0.8531487

test_carprice$error <- test_carprice$price - test_carprice$test_price

# Plot the Error
ggplot(data=test_carprice, aes(x=horsepower,y=error)) + geom_point() +
  scale_x_continuous(name = "horsepower", breaks = seq(60,160,20), limits = c(60,160)) +
  geom_hline(yintercept = 0)

# Result - The errors are scattered and it proves the model is good

# Plot - Actual vs Predicted with horsepower field
ggplot(test_carprice, aes(horsepower, price)) + geom_line(aes(colour = "blue" )) + 
  scale_x_continuous(name = "horsepower", breaks = seq(60,160,10), limits = c(60,160)) + 
  scale_y_continuous(name = "Price", breaks = seq(0,50000,5000), limits = c(0,50000)) + 
  geom_line(aes(x=horsepower, y=test_price, colour="red")) +
  scale_fill_discrete(name="Result", labels=c("red", "blue"))

# Result - The actual and predicted are overlapping well.

# Plot - Actual vs Predicted with stroke
ggplot(test_carprice, aes(stroke, price)) + geom_line(aes(colour = "blue" )) + 
  scale_x_continuous(name = "stroke", breaks = seq(2,4.5,0.5), limits = c(2,4.5)) + 
  scale_y_continuous(name = "Price", breaks = seq(0,50000,5000), limits = c(0,50000)) + 
  geom_line(aes(x=stroke, y=test_price, colour="red"))

#################################################################################
#                           Conclusion                                          #
#################################################################################
# As per the analysis the model 22 is found the best sutiable model having      #
# the following independent variables.                                          #
#   enginelocation, carwidth, curbweight, enginesize, stroke, carbrandbmw,      #
#   carbrandbuick, carbrandjaguar, carbrandpeugeot, carbrandsubaru,             #
#   carbrandtoyota, cylindernumberfive, cylindernumberfour, cylindernumbersix   #
#                                                                               #
# This model provides the following results:                                    #
#   Multiple R-squared:  0.9584,	Adjusted R-squared:  0.9539                   #
#                                                                               #
# Model Evaluation provides the R-square as 0.8531487                           #
#                                                                               #
# The above provided plots proves that this model is a good fit.                #
#                                                                               #
# Note: After this model, I had tried other models by removing the              #  
# variables from the model 22, however none of them gave the result as like     #  
# model_22. Hence model_22 is the best fit model.                               #
#                                                                               #
#             Thanks!                                                           #
#################################################################################
