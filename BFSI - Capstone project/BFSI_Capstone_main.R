###################################################################
#                  BFSI Capstone - Project                        #
###################################################################
#                     Group members                               #     
# Shivam Kakkar (Facilitator) - Roll Number - DDA1730346          #
# Ashwin Suresh                                                   #
# Manohar Shanmugasundaram                                        #
# P Sai Prathyusha                                                #
###################################################################

# Loading the required libraries for this assignment problem
library(ggplot2)
library(caret)
library(caTools)
library(dummies)
library(MASS)
library(car)
library(dplyr)
library(corrplot)
library(StatMeasures)
library(ROCR)
library(cowplot)
library("ROSE")
library(scorecard)
library(randomForest)
library(rpart)
library(rpart.plot)
library(DMwR)
library(Information)

#######################################################################################
#section 1
###################################################################################
# Load the Demographic data in the working environment 
inspect_demo_1 <- read.csv("Demographic data.csv",na.strings="")

# Load the credit bureau data in the working environment 
inspect_credit_1 <- read.csv("Credit Bureau data.csv",na.strings="")

colSums(is.na(inspect_credit_1))
which(is.na(inspect_credit_1$No.of.trades.opened.in.last.6.months))

colSums(is.na(inspect_demo_1))
summary(inspect_demo_1)
summary(inspect_credit_1)
head(inspect_demo_1)
head(inspect_credit_1)
#############################
#Check for duplicated records
#############################

sum(duplicated(inspect_demo_1$Application.ID)) #3
sum(duplicated(inspect_credit_1$Application.ID)) #3

# Identify the rows where duplicates present
which(duplicated(inspect_demo_1$Application.ID)) # 27587 42638 59023

# Identify the rows where duplicates present
which(duplicated(inspect_credit_1$Application.ID))# 27587 42638 59023

# Removing the duplicates from both the datasets
index_duplicate<-which(duplicated(inspect_demo_1$Application.ID))
inspect_demo_1<-inspect_demo_1[-c(index_duplicate),]
dim(inspect_demo_1) # 71292    12

index_duplicate1<-which(duplicated(inspect_credit_1$Application.ID))
inspect_credit_1<-inspect_credit_1[-c(index_duplicate1),]
dim(inspect_credit_1) #71292    19

inspect_demo_1$Performance.Tag<-as.factor(ifelse(inspect_demo_1$Performance.Tag==1,"bad","good"))
inspect_credit_1$Performance.Tag<-as.factor(ifelse(inspect_credit_1$Performance.Tag==1,"bad","good"))

###############################
# Checking NA or missing values
###############################

colSums(is.na(inspect_demo_1))

#######################
# Checking for Outliers (GRAPHS TO BE INCLUDED)
#######################

# Age column
outlier <- outliers(inspect_demo_1$Age)
outlier$numOutliers  #20
sort(inspect_demo_1$Age[outlier$idxOutliers]) # -3  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
test <- quantile(inspect_demo_1$Age,seq(0,1,0.01))
test
boxplot(test)

# Chnaging the value of -3 to 27
inspect_demo_1[(which(inspect_demo_1$Age<=0)),]$Age <- 27

# No of dependents column

outliers <- outliers(inspect_demo_1$No.of.dependents)
outliers$numOutliers  #0

###########################

#income
test <- quantile(inspect_demo_1$Income,seq(0,1,0.01)) 
test
boxplot(test) 

inspect_demo_1[(which(inspect_demo_1$Income<=0.00)),]$Income <- 4.50

# No of months in current residence column
test <- quantile(inspect_demo_1$No.of.months.in.current.residence,seq(0,1,0.01)) 
test # no outlier
boxplot(test)

# No of months in current residence column
test <- quantile(inspect_demo_1$No.of.months.in.current.company,seq(0,1,0.01)) # 133.0
test 
inspect_demo_1[(which(inspect_demo_1$No.of.months.in.current.company>74)),]$No.of.months.in.current.company<-74
summary(inspect_demo_1$No.of.months.in.current.company)

##################################
# derived columns for age & income
##################################
# HISTOGRAM FOR INCOME
hist(inspect_demo_1$Income , xlab = "Income" , main = "Distribution of Income");

# From the histogram we see that creating bins as a derived column will be better

##############################################
# 1. Create a bin based on the income
#     Income group        Annual income
#     -------------       --------------
#     <=10                 <= 10.00
#     10 to 20            > 10.00 and <= 20.00
#     20 to 30            > 20.00 and <= 30.00
#     30 to 40            > 30.00 and <= 40.00
#     40 to 50            > 40.00 and <= 50.00
#     >50                 > 50.00
##############################################

inspect_demo_1$Income_grp <- cut((inspect_demo_1$Income), 
                                 breaks=c(0,10.00,20.00,30.00,40.00,50.00,60.00), 
                                 labels=c("<=10","10 to 20","20 to 30","30 to 40","40 to 50",
                                          ">50"),include.lowest=T)

#=============================================
# HISTOGRAM FOR AGE
hist(inspect_demo_1$Age, xlab = "Age" , main = "Distribution of Age")

# From the histogram we see that creating bins as a derived column will be better

##############################################
# 2. Create a bin based on the Age
#     Age group           Age
#     -------------       --------------
#     <=20                 <= 20
#     20 to 30            > 20 and <= 30
#     30 to 40            > 30 and <= 40
#     40 to 50            > 40 and <= 50
#     50 to 60            > 50 and <= 60
#     >60                 > 60
##############################################

inspect_demo_1$Age_grp <- cut((inspect_demo_1$Age), 
                              breaks=c(0,20.00,30.00,40.00,50.00,60.00,65.00), 
                              labels=c("<=20","20 to 30","30 to 40","40 to 50","50 to 60",
                                       ">60"),include.lowest=T)

####################################################################
# HISTOGRAM FOR NO_OF_MONTHS_IN_CURRENT_COMPANY
hist(inspect_demo_1$No.of.months.in.current.company , xlab = "No_of_months_in_current_company" , main = "Distribution for no of months in current company")

####################################################################
# 3. Create a bin based on the No of months in current company
#     No of months group       No of months in current company
#     -------------            --------------
#     <=25                      <= 25
#     25 to 50                  > 25 and <= 50
#     50 to 100                 > 50 and <= 100
#     >100                      >100
####################################################################

inspect_demo_1$No_mnths_company_grp <- cut((inspect_demo_1$No.of.months.in.current.company), 
                                           breaks=c(0,25,50,100,150), 
                                           labels=c("<=25","25 to 50","50 to 100",">100"),include.lowest=T)
################################################################################################################

inspect_demo_1$No.of.dependents <- as.factor(inspect_demo_1$No.of.dependents)
table(inspect_demo_1$Performance.Tag)

# bad  good 
# 2947 66920 

#####################################################
# Removing variables for which we have created bins
#####################################################
inspect_demo_1$Income <- NULL
inspect_demo_1$Age <- NULL
inspect_demo_1$No.of.months.in.current.company <- NULL

###################################################################
#  section 2  +++++++++ggplots for demographic
# #################################################################

###########################
# Exploratory Data Analysis
###########################

######################################################################
# Now we will use only the data where "Performance.Tag" is not missing
######################################################################
rejected_demo <- inspect_demo_1[is.na(inspect_demo_1$Performance.Tag),]

inspect_demo_1 <- inspect_demo_1[!is.na(inspect_demo_1$Performance.Tag),]
sum(is.na(inspect_demo_1$Performance.Tag)) #0

####################################################
# Function to generate plot for categorical variable
####################################################

gen_univariate_plot_categorical <- function(df , var , title , xlab) {
  ggplot( data = df , aes_string(x = var)) + 
    geom_bar(aes(y = (..count..)/sum(..count..)) ,fill="green",col = "red" , alpha = 0.5 , position = "dodge") +
    geom_text(aes(y = ((..count..)/sum(..count..)), label = scales::percent((..count..)/sum(..count..))), stat = "count" , position = position_dodge(width = 1) , vjust = -0.9 , size = 2.6) +
    scale_y_continuous(labels = function(x){ paste0(x*100, "%") }) +
    labs(title = title , x = xlab , y = "Percentage", fill = "Performance.Tag") +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.6))
}

#  Lets see the distribution of dependent variable
gen_univariate_plot_categorical(inspect_demo_1 , "Performance.Tag" , "Default status" , "Performance Tag") 

#################################################################
# There is only 4.2% of bad data and 95.8% of good data available
# Performing the analysis only on the bad data
#################################################################

p1 <- gen_univariate_plot_categorical(inspect_demo_1[inspect_demo_1$Performance.Tag=="bad",] , "Age_grp" , "Age Distribution for defaulters" , "Age Group") 
p2 <- gen_univariate_plot_categorical(inspect_demo_1[inspect_demo_1$Performance.Tag=="bad",] , "Gender"  , "Gender Distrbution for defaulters" , "Gender") 
p3 <- gen_univariate_plot_categorical(inspect_demo_1[inspect_demo_1$Performance.Tag=="bad",] , "Marital.Status..at.the.time.of.application." , "Marital Status for defaulters"  , "Marital Status of applicant")
p4 <- gen_univariate_plot_categorical(inspect_demo_1[inspect_demo_1$Performance.Tag=="bad",] , "Income_grp" , "Income Distribution for defaulters" , "Income Group") 
p5 <- gen_univariate_plot_categorical(inspect_demo_1[inspect_demo_1$Performance.Tag=="bad",] , "Education" , "Education Distribution for defaulters" , "Education") 
p6 <- gen_univariate_plot_categorical(inspect_demo_1[inspect_demo_1$Performance.Tag=="bad",] , "Profession" , "Profession Distribution for defaulters" , "Profession") 
p7 <- gen_univariate_plot_categorical(inspect_demo_1[inspect_demo_1$Performance.Tag=="bad",] , "Type.of.residence" , "Residence Type Distribution for defaulters" , "Residence") 
p8 <-  histogram(inspect_demo_1$No.of.months.in.current.residence[inspect_demo_1$Performance.Tag=="bad"])
p9 <- gen_univariate_plot_categorical(inspect_demo_1[inspect_demo_1$Performance.Tag=="bad",] , "No_mnths_company_grp" , "No of months in company distribution" , "No of months in current company group")
p10 <-gen_univariate_plot_categorical(inspect_demo_1[inspect_demo_1$Performance.Tag=="bad",] , "No.of.dependents" , "No of dependents distribution" , "No of dependents") 

############################################
# Plotting the above created plots in a grid.
############################################

plot_grid(p1,p2 , p3 , p4)
# From the above plots, we could see Male, Married and age groups between 20 to 60 are high defaulters.

plot_grid(p5,p6 , p7)
# From the above plots, we could see Rented residents and Salaried professionals are high defaulters.

plot(p8)
plot_grid(p9,p10)

table(inspect_demo_1$Performance.Tag)

# good    bad
# 66920  2947

########################################################################
# Section 3 demographic data split into train and test
########################################################################

# Saving the backup of the dataset inspect_demo_1
inspect_demo_original <- inspect_demo_1

# Removing the Application ID as it is of no use in Model Building
inspect_demo_1$Application.ID <- NULL

inspect_demo_1$Performance.Tag <- ifelse(inspect_demo_1$Performance.Tag == "bad" , 1 , 0)
indices = sample.split(inspect_demo_1$Performance.Tag, SplitRatio = 0.7)

# train data
train <- inspect_demo_1[indices,]
sum(train$Performance.Tag)/nrow(train) #0.04

table(train$Performance.Tag)
#   0     1 
# 46844  2063 

# test data
test <- inspect_demo_1[!(indices),]
sum(test$Performance.Tag)/nrow(test) #0.04

###################################################################
# Section 4  Calculating the Information value of the train dataset
####################################################################
# Calculating the Information value of the train dataset

iv_train <- iv(train, y = 'Performance.Tag') %>% mutate( info_value = round(info_value,  3))
iv_train

#                                     variable info_value
#1            No.of.months.in.current.residence      0.161
#2                                   Income_grp      0.042
# Removing the following predictors which have the information value less than 0.02
#3                         No_mnths_company_grp      0.011
#4                             No.of.dependents      0.004
#5                                    Education      0.002
#6                                   Profession      0.002
#7                            Type.of.residence      0.002
#8                                       Gender      0.002
#9                                      Age_grp      0.001
#10 Marital.Status..at.the.time.of.application.      0.001

# IV value
IV <- create_infotables(data=train, y="Performance.Tag", bins=10, parallel=FALSE)
plot_infotables(IV, IV$Summary$Variable[1:3] , same_scales = FALSE)
plot_infotables(IV, IV$Summary$Variable[4:6] , same_scales = FALSE)

train_sub <- train[,c("No.of.months.in.current.residence" , "Income_grp", "Performance.Tag")]

test_sub <- test[,c("No.of.months.in.current.residence" , "Income_grp", "Performance.Tag")]

IV <- create_infotables(data=train_sub, y="Performance.Tag", bins=10, parallel=FALSE)

p1 <- plot_infotables(IV, "No.of.months.in.current.residence") # Mostly Monotonic
p2 <- plot_infotables(IV, "Income_grp") # Monotonic

# will try to make it monotonic but in the process of making it monotonic we are doing lot of data clumping

plot_grid(p1,p2)

bins_train <- woebin(train_sub, y = 'Performance.Tag')

train_sub_woe <- woebin_ply(train_sub , bins_train)
test_sub_woe <- woebin_ply(test_sub , bins_train)

# ##########################################################
# Section 5 .To address imbalanced classificaion for Demographic data
# ##########################################################
# ################    DEMOGRAPHIC DATA HAS  ONLY A PARTIAL INFLUENCE ON DEFAULTERS.As such, we are preparing only a basic model using it,
# ################    without optimising much
# ###############    SO TO GET THE CORRECT PREDICTION WE WILL RUN DIFFERENT SAMPLING METHODS ON THE COMBINED DATASET LATER
# ################  FURTHER, THE COMBINED DATASET  for various sampling techniques  WILL BE USED TO PREPARE SCORECARD & ANALYSIS.

# Original train dataset
inspect_demo_orig_woe <- train_sub_woe

table(inspect_demo_orig_woe$Performance.Tag)

#   0     1 
# 46844  2063 

# ############################################################################################
# Section 6. MODEL FOR DEMOGRAPHIC DATA
# ############################################################################################
##############################################################################################
# Building the logistic regression model on the demographic data  
# inspect_demo_orig_woe 
###############################################################################################

str(inspect_demo_orig_woe)

##############################
# Model with original data
##############################
model_1 <-glm(Performance.Tag~ No.of.months.in.current.residence_woe + Income_grp_woe,
              family = "binomial",data = inspect_demo_orig_woe)
summary(model_1)

# This is the final model for the original train data.
model_inspect_demo_orig_woe <- model_1

model_inspect_demo_orig_woe

# Call:  glm(formula = Performance.Tag ~ No.of.months.in.current.residence_woe + 
# Income_grp_woe, family = "binomial", data = inspect_demo_orig_woe)

# Coefficients:
#  (Intercept)  No.of.months.in.current.residence_woe                         Income_grp_woe  
#     -3.1227                                 0.9369                                 0.8130  

# Degrees of Freedom: 48906 Total (i.e. Null);  48904 Residual
# Null Deviance:	    17100 
# Residual Deviance: 16900 	AIC: 16900

test_sub_woe

############################################
# Predicting for the "test_sub_woe"
############################################

test <- test_sub_woe
test_pred <- predict(model_inspect_demo_orig_woe, type = "response", newdata = test)

test_actual_default <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))

#####################
#summary of test_pred
#####################

summary(test_pred)
test$prob <- test_pred

############################################
# To find out the optimal probalility cutoff
############################################

perform_fn <- function(cutoff) 
{
  predicted_default <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(predicted_default, test_actual_default, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

s = seq(0.01979, 0.08884,length=100)
OUT = matrix(0,100,3)

for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0.01,0.1,length=5),seq(0.01,0.1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.02, 0.5,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

cutoff <- min(s[which(abs(OUT[,1]-OUT[,2])<0.04)])
cutoff # 0.0435

test_pred_default <- factor(ifelse(test$prob >= cutoff, "Yes", "No"))
test_actual_default <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))

conf <- confusionMatrix(test_pred_default, test_actual_default, positive = "Yes")

conf$overall[1]  #-- Accuracy : 60.64%
conf$byClass[1]  #-- Sensistivity : 57.69%
conf$byClass[2]  # -- Specificity : 60.77%

####################################################

# ########################################################################
#===================CREDIT DATA CLEANING AND PREPARATION #================
##########################################################################
# NO HIT APPLICANTS

index_NA_Avgas.CC.Utilization.in.last.12.months<- which(is.na(inspect_credit_1$Avgas.CC.Utilization.in.last.12.months)) # #1058
x<-inspect_credit_1[index_NA_Avgas.CC.Utilization.in.last.12.months,]
table(x$Performance.Tag)
#bad good 
# 48  975

#############################################
# Section 7 (credit data outlier treatment)
#############################################################
sapply(inspect_credit_1,function(a)unique(a))

sapply(inspect_credit_1[,-c(1,19)], function(x) quantile(x, seq(0, 1, 0.01),na.rm = T))
sapply(inspect_credit_1[,-c(1,19)], function(x)boxplot.stats(x)$out)

# Average CC utilisation column

outlier <- outliers(inspect_credit_1$Avgas.CC.Utilization.in.last.12.months)
outlier$numOutliers  #3486
sort(unique(inspect_credit_1$Avgas.CC.Utilization.in.last.12.months[outlier$idxOutliers]))

# 104 105 106 107 108 109 110 111 112 113

# Usually we don't see Avg CC utilization to be greater than , hence we are capping the values greater than 100 to 100
inspect_credit_1$Avgas.CC.Utilization.in.last.12.months [which(inspect_credit_1$Avgas.CC.Utilization.in.last.12.months > 100)] = 100

boxplot.stats(inspect_credit_1$Avgas.CC.Utilization.in.last.12.months)$out
length(boxplot.stats(inspect_credit_1$Avgas.CC.Utilization.in.last.12.months)$out)
boxplot(inspect_credit_1$Avgas.CC.Utilization.in.last.12.months)
test <- quantile(inspect_credit_1$Avgas.CC.Utilization.in.last.12.months,seq(0,1,0.01),na.rm=T)

c1 <- ggplot(inspect_credit_1, aes("Avgas.CC.Utilization.in.last.12.months", Avgas.CC.Utilization.in.last.12.months,fill="brown",alpha=0.2)) +
  geom_boxplot(outlier.colour="blue",outlier.alpha = .2,outlier.shape=17) +
  scale_y_log10(breaks = quantile(inspect_credit_1$Avgas.CC.Utilization.in.last.12.months,na.rm = T))

c2 <- ggplot(inspect_credit_1, aes("Avgas.CC.Utilization.in.last.12.months", Avgas.CC.Utilization.in.last.12.months,fill="brown",alpha=0.2)) +
  geom_point() +
  geom_violin() +
  scale_y_log10(breaks = quantile(inspect_credit_1$Avgas.CC.Utilization.in.last.12.months,na.rm = T))

gridExtra::grid.arrange(c1, c2, ncol = 2)

# Outstanding balance
boxplot.stats(inspect_credit_1$Outstanding.Balance)$out
boxplot(inspect_credit_1$Outstanding.Balance) #There are no outliers

############################################################
histogram(inspect_credit_1$Avgas.CC.Utilization.in.last.12.months)

inspect_credit_1$Avgas_CC_bin <- cut((inspect_credit_1$Avgas.CC.Utilization.in.last.12.months), 
                                     breaks=c(0,20.00,40.00,60.00,80.00,100.00,110.00), 
                                     labels=c("<=20","20 to 40","40 to 60","60 to 80","80 to 100",
                                              ">100"),include.lowest=T, na.rm = TRUE)
#######################################################################
histogram(inspect_credit_1$Outstanding.Balance)
quantile(inspect_credit_1$Outstanding.Balance, seq(0, 1, 0.25),na.rm = T)

inspect_credit_1$Outstanding_Balance_bin <- as.factor(cut(inspect_credit_1$Outstanding.Balance, 
                                                          breaks = c(0,210000, 770000, 2920000, 5210000,5218801),
                                                          labels=c("<=210000","210000 to 770000","770000 to 2920000","2920000 to 5210000",
                                                                   ">5218801"),include.lowest=T, na.rm = TRUE))

####################################################################
# FACTORING VARIABLES inspect_credit_1
####################################################################

inspect_credit_1$No.of.times.90.DPD.or.worse.in.last.6.months <- as.factor(inspect_credit_1$No.of.times.90.DPD.or.worse.in.last.6.months)
inspect_credit_1$No.of.times.60.DPD.or.worse.in.last.6.months <- as.factor(inspect_credit_1$No.of.times.60.DPD.or.worse.in.last.6.months)
inspect_credit_1$No.of.times.30.DPD.or.worse.in.last.6.months <- as.factor(inspect_credit_1$No.of.times.30.DPD.or.worse.in.last.6.months)

inspect_credit_1$No.of.times.90.DPD.or.worse.in.last.12.months <- as.factor(inspect_credit_1$No.of.times.90.DPD.or.worse.in.last.12.months)
inspect_credit_1$No.of.times.60.DPD.or.worse.in.last.12.months <- as.factor(inspect_credit_1$No.of.times.60.DPD.or.worse.in.last.12.months)
inspect_credit_1$No.of.times.30.DPD.or.worse.in.last.12.months <- as.factor(inspect_credit_1$No.of.times.30.DPD.or.worse.in.last.12.months)

inspect_credit_1$No.of.PL.trades.opened.in.last.12.months <- as.factor(inspect_credit_1$No.of.PL.trades.opened.in.last.12.months)
inspect_credit_1$No.of.PL.trades.opened.in.last.6.months <- as.factor(inspect_credit_1$No.of.PL.trades.opened.in.last.6.months)

inspect_credit_1$No.of.trades.opened.in.last.6.months <- as.factor(inspect_credit_1$No.of.trades.opened.in.last.6.months)

inspect_credit_1$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans. <- as.factor(inspect_credit_1$No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)
inspect_credit_1$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. <- as.factor(inspect_credit_1$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)

inspect_credit_1$Presence.of.open.home.loan <- as.factor(inspect_credit_1$Presence.of.open.home.loan)
inspect_credit_1$Presence.of.open.auto.loan <- as.factor(inspect_credit_1$Presence.of.open.auto.loan)

################################################################################################################################

#remove variables for which bins rae created
inspect_credit_1$Outstanding.Balance <- NULL
inspect_credit_1$Avgas.CC.Utilization.in.last.12.months <- NULL
table(inspect_credit_1$Performance.Tag)

#################################################################################
# DATA SET FOR REJECTED INFERENCE
#################################################################################

inspect_demo_2<-inspect_demo_1[which(!is.na(inspect_demo_1$Performance.Tag)),]
dim(inspect_demo_2) #69867    12

table(inspect_demo_2$Performance.Tag)
rejected_credit<-inspect_credit_1[which(is.na(inspect_credit_1$Performance.Tag)),]
inspect_credit_2<-inspect_credit_1[which(!is.na(inspect_credit_1$Performance.Tag)),]
dim(inspect_credit_2)# 69867    19

######################
# Univariate Analysis
######################

# Lets see the distribution of dependent variable
gen_univariate_plot_categorical(inspect_credit_1 , "Performance.Tag" , "Default status" , "Performance Tag") 

# There is only 4.1% of bad data, 93.9% of good  and 2.0% of 'NA' data available
str(inspect_credit_1)

p1 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "Avgas_CC_bin" , "Average Credit Utilization Distribution for defaulters" , "Avg Credit Utilization") 
p2 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "Outstanding_Balance_bin" , "Outstanding Balance Distribution for defaulters" , "Outstanding balance")
p3 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "as.factor(No.of.times.90.DPD.or.worse.in.last.6.months)" , "90 DPD in 6 mnths" , "no_of_times")
p4 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "as.factor(No.of.times.60.DPD.or.worse.in.last.6.months)" , "60 DPD in 6 mnths" , "no_of_times")
p5 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "as.factor(No.of.times.30.DPD.or.worse.in.last.6.months)" , "30 DPD in 6 mnths", "no_of_times")
p6 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "as.factor(No.of.times.90.DPD.or.worse.in.last.12.months)" , "90 DPD in 12 mnths" , "no_of_times")
p7 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "as.factor(No.of.times.60.DPD.or.worse.in.last.12.months)" , "60 DPD in 12 mnths" , "no_of_times")
p8 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "as.factor(No.of.times.30.DPD.or.worse.in.last.12.months)" , "30 DPD in 12 mnths", "no_of_times")
p9 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "as.factor(No.of.trades.opened.in.last.6.months)" , "Trades in last 6 months" , "no_of_trades")
p10 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "as.factor(No.of.trades.opened.in.last.12.months)" , "Trades in last 12 months" , "no_of_trades") 
p11 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "as.factor(No.of.PL.trades.opened.in.last.6.months)" , "PL trades in last 6 months" , "no_of_trades")
p12 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "as.factor(No.of.PL.trades.opened.in.last.12.months)" , "PL trades in last 12 months" , "no_of_trades")
p13 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "as.factor(No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.)" , "Inquiries in 6 months" , "no_of_inquiries")
p14 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "as.factor(No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.)" , "Inquiries in 12 months" , "no_of_inquiries")
p15 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "as.factor(Presence.of.open.home.loan)" , "Distribution of open home loan for defaulters" , "open home loan")
p16 <- gen_univariate_plot_categorical(inspect_credit_1[inspect_credit_1$Performance.Tag=="bad",] , "as.factor(Presence.of.open.auto.loan)" , "Distribution of open auto loan for defaulters" , "open auto loan")

plot_grid(p1,p2)
plot_grid(p3,p4,p5,p6,p7,p8)
plot_grid(p9,p10,p11,p12)
plot_grid(p13,p14,p15,p16)

#########################################################################################################
#########################################################################################################
# Merging  demo and credit files
##################################################################################
merged_file_1 <- cbind(inspect_demo_2 , inspect_credit_2 )
colSums(is.na(merged_file_1))

table(merged_file_1$Performance.Tag)
#good   bad
#66920  2947

###############################################################

######################################################################################################
# section 8. splitting merged file(merged_file_1) into train and test and for rejected inference
#################################################################################################

set.seed(342)
indices <- sample.split(merged_file_1$Performance.Tag, SplitRatio = 0.7)
train <- merged_file_1[indices,]
test <- merged_file_1[!(indices),]
colnames(test)
#check proportion in test
table(test$Performance.Tag) #884 20076
table(train$Performance.Tag) #2063 46844
###############################################################################
##########################################################
# Merging demo and credit for test and rejected population
############################################################
rejected_merged<-cbind(rejected_demo , rejected_credit)
colnames(rejected_merged)

rejected_merged<-rejected_merged[,-c(1,9)]
colnames(rejected_merged)

table(rejected_merged$Performance.Tag)
View(rejected_merged)

colnames(test)
test<-test[,-c(8)]

test_and_rejected<-rbind(rejected_merged,test)
dim(test_and_rejected)#22385    29

####################################################
# Section 9 IV for merged
#####################################################

train_iv = iv(train, y = 'Performance.Tag') %>% mutate(info_value = round(info_value,3))
train_iv

#                                                         variable info_value
#1                            No.of.trades.opened.in.last.12.months      0.314
#2                         No.of.PL.trades.opened.in.last.12.months      0.296
#3  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.      0.281
#4                                                     Avgas_CC_bin      0.281
#5                                               Total.No.of.Trades      0.276
#6                     No.of.times.30.DPD.or.worse.in.last.6.months      0.254
#7                          No.of.PL.trades.opened.in.last.6.months      0.227
#8                     No.of.times.60.DPD.or.worse.in.last.6.months      0.225
#9                    No.of.times.90.DPD.or.worse.in.last.12.months      0.221
#10                   No.of.times.30.DPD.or.worse.in.last.12.months      0.221
#11  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.      0.201
#12                   No.of.times.60.DPD.or.worse.in.last.12.months      0.196
#13                            No.of.trades.opened.in.last.6.months      0.189
#14                                         Outstanding_Balance_bin      0.177
#15                    No.of.times.90.DPD.or.worse.in.last.6.months      0.175
#16                               No.of.months.in.current.residence      0.157
#17                                                      Income_grp      0.035
# Reject the following because IV < 0.02
#18                                      Presence.of.open.home.loan      0.019
#19                                            No_mnths_company_grp      0.012
#20                                                No.of.dependents      0.004
#21                                                       Education      0.002
#22                                               Type.of.residence      0.002
#23                                                          Gender      0.001
#24                     Marital.Status..at.the.time.of.application.      0.001
#25                                                      Profession      0.001
#26                                                         Age_grp      0.001
#27                                      Presence.of.open.auto.loan      0.001
#28                                                  Application.ID      0.000

####################################################################################
# section 10. selecting variables in train and test based on IV
################################################################################
select_dt_Train<-train[,c("No.of.trades.opened.in.last.12.months",
                          "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
                          "No.of.PL.trades.opened.in.last.12.months",
                          "Avgas_CC_bin",
                          "Total.No.of.Trades",
                          "No.of.times.30.DPD.or.worse.in.last.6.months",
                          "No.of.PL.trades.opened.in.last.6.months",
                          "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
                          "No.of.times.90.DPD.or.worse.in.last.12.months",
                          "No.of.times.30.DPD.or.worse.in.last.12.months",
                          "No.of.times.60.DPD.or.worse.in.last.6.months",
                          "No.of.trades.opened.in.last.6.months",
                          "No.of.times.60.DPD.or.worse.in.last.12.months",
                          "Outstanding_Balance_bin",
                          "No.of.months.in.current.residence",
                          "No.of.times.90.DPD.or.worse.in.last.6.months",
                          "Income_grp",
                          "Performance.Tag")]


#######################################################################################
select_dt_test<-test[,c("No.of.trades.opened.in.last.12.months",
                        "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
                        "No.of.PL.trades.opened.in.last.12.months",
                        "Avgas_CC_bin",
                        "Total.No.of.Trades",
                        "No.of.times.30.DPD.or.worse.in.last.6.months",
                        "No.of.PL.trades.opened.in.last.6.months",
                        "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
                        "No.of.times.90.DPD.or.worse.in.last.12.months",
                        "No.of.times.30.DPD.or.worse.in.last.12.months",
                        "No.of.times.60.DPD.or.worse.in.last.6.months",
                        "No.of.trades.opened.in.last.6.months",
                        "No.of.times.60.DPD.or.worse.in.last.12.months",
                        "Outstanding_Balance_bin",
                        "No.of.months.in.current.residence",
                        "No.of.times.90.DPD.or.worse.in.last.6.months",
                        "Income_grp",
                        "Performance.Tag")]

#############################################################################
#selecting variables in test_and_rejected based on IV (for reject inference)
#############################################################################

select_dt_test_and_rejected<-test_and_rejected[,c("No.of.trades.opened.in.last.12.months",
                                                  "No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.",
                                                  "No.of.PL.trades.opened.in.last.12.months",
                                                  "Avgas_CC_bin",
                                                  "Total.No.of.Trades",
                                                  "No.of.times.30.DPD.or.worse.in.last.6.months",
                                                  "No.of.PL.trades.opened.in.last.6.months",
                                                  "No.of.Inquiries.in.last.6.months..excluding.home...auto.loans.",
                                                  "No.of.times.90.DPD.or.worse.in.last.12.months",
                                                  "No.of.times.30.DPD.or.worse.in.last.12.months",
                                                  "No.of.times.60.DPD.or.worse.in.last.6.months",
                                                  "No.of.trades.opened.in.last.6.months",
                                                  "No.of.times.60.DPD.or.worse.in.last.12.months",
                                                  "Outstanding_Balance_bin",
                                                  "No.of.months.in.current.residence",
                                                  "No.of.times.90.DPD.or.worse.in.last.6.months",
                                                  "Income_grp",
                                                  "Performance.Tag")]
dim(select_dt_test_and_rejected) #22385    18

#############################################################################
#############################################################################
# Section 11. Train and test woe
############################################################################
set.seed(231)
woe<-woebin(select_dt_Train, y = 'Performance.Tag')
woe%>%  knitr::kable()

names(woe)
###########################################################################
#  Section 12. MAKING NON-MONOTONIC WOE TO MONOTONIC
###########################################################################
woe$No.of.Inquiries.in.last.12.months..excluding.home...auto.loans. #(var1)

woe$Total.No.of.Trades#var2

select_dt_Train$Performance.Tag<-as.factor(ifelse(select_dt_Train$Performance.Tag==1,"bad","good"))

table(select_dt_Train$Performance.Tag)

###########################################
var1 <- select_dt_Train[,c("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.","Performance.Tag")]

woe[c("No.of.Inquiries.in.last.12.months..excluding.home...auto.loans.")]<-
  woebin(var1, y = 'Performance.Tag',max_num_bin = 2)

###############################################
var2 <- select_dt_Train[,c("Total.No.of.Trades","Performance.Tag")]

woe[c("Total.No.of.Trades")]<-
  woebin(var2, y = 'Performance.Tag',max_num_bin = 2)

# COMMENT: With the above, we have majority of the variables(13/17). This adds to the stability of the model
#         The stability is again addressed , while fitting various models . The models are
#         tuned so that the final model selected will be stable and generalisable.
#         
#         Also, reducing the number of bins  to make it monotonic , results in 
#         loss of information . There  is always a trade off between accuracy and loss of information
#         Finally, we have decided not to bin the remaining 4 variables out of 17 and address the issue 
#         during model building.
#========================================================================
table(select_dt_Train$Performance.Tag)

########################################################################
# section 13. REPLACING train and test(of the merged) BY WOE values
########################################################################
set.seed(342)
table(select_dt_test$Performance.Tag)
final_train <- woebin_ply(select_dt_Train, woe)
final_train$Performance.Tag<-as.factor(ifelse(final_train$Performance.Tag=="bad",1,0))
final_test<-woebin_ply(select_dt_test, woe)

colSums(is.na(final_test))
which(is.na(final_test$ No.of.times.30.DPD.or.worse.in.last.12.months_woe))

final_test<-final_test[-20693,]
dim(final_test) # 20959    18

################################################################################
# section 14.  REPLACING test_and_rejected by woe values (for reject inference)
################################################################################
table(select_dt_test_and_rejected$Performance.Tag)


test_and_rejected_woe<-woebin_ply(select_dt_test_and_rejected, woe)
colSums(is.na(test_and_rejected_woe))
which(is.na(test_and_rejected_woe$No.of.times.30.DPD.or.worse.in.last.12.months_woe))
test_and_rejected_woe<-test_and_rejected_woe[-c(711,22118),]

colSums(is.na(test_and_rejected_woe))
#############################################################################
# section 15.. woe data set (df_rejected) only for rejected population
#############################################################################
index_rejected<-which(is.na(test_and_rejected_woe$Performance.Tag))
df_rejected<-test_and_rejected_woe[index_rejected,]
head(df_rejected)
dim(df_rejected)
colSums(is.na(df_rejected))
which(is.na(test_and_rejected_woe$No.of.times.30.DPD.or.worse.in.last.12.months_woe))
#################################################################################################

############################################################################
# section 17: 
# DIFFERENT MODELS for COMBINED DATA: LOGISTIC , DECISION AND RANDOM FOREST
# WITH VARIOUS METHODS OF SAMPLING
############################################################################ 

############################################################################### 
#   To overcome the imbalance in the data set (Performance.Tag field).        # 
#   We used the following methods to find the better approach                 #
#   1) Over sampling                                                          #
#   2) Over sampling using SMOTE algorithm                                    #
#   3) Under sampling                                                         #
############################################################################### 

# 1) Over sampling
##################

table(final_train$Performance.Tag)

# 0     1 
# 46844  2063 

ov_final_train <- ovun.sample(Performance.Tag~.,data=final_train,method ="over",N=90000)$data
table(ov_final_train$Performance.Tag)

# 0     1 
# 46844 43156 

# 2) over sampling using SMOTE algorithm
########################################

final_train$Performance.Tag <- as.factor(final_train$Performance.Tag)
ov_smote_final_train <- SMOTE(Performance.Tag~.,data=final_train, perc.over = 1000,perc.under=100)
table(ov_smote_final_train$Performance.Tag)

#     0     1 
#   20630 22693 

# 3) Under sampling
###################

un_final_train <- ovun.sample(Performance.Tag~.,data=final_train,method ="under")$data
table(un_final_train$Performance.Tag)

# 0    1 
# 2112 2063 

str(final_train)

# ############################################################################ 
# ############## BUILDING LOGISTIC REGRESSION FOR MERGED DATASET #############
# # We are building 4 Logistic models
# #   1)original data without sampling
# #   2)over sampling
# #   3)over sampling through SMOTE
# #   4)under sampling

################################################################################
# 1)original data without sampling
################################################################################
orig_model_1 <- glm(formula = Performance.Tag ~ ., family = "binomial", 
                    data = final_train)

summary(orig_model_1)

# Performing StepAIC
orig_model_2 <- stepAIC(orig_model_1,direction = "both")
summary(orig_model_2)
data.frame(sort(vif(orig_model_2)))

# # Removing variables greater than vif 2 and insignicant probabilities. 
# 
# No.of.times.30.DPD.or.worse.in.last.6.months_woe
orig_model_3 <- glm(formula = Performance.Tag ~ No.of.trades.opened.in.last.12.months_woe + 
                    No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    Avgas_CC_bin_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe, 
                    family = "binomial", data = final_train)
summary(orig_model_3)
data.frame(sort(vif(orig_model_3)))
 
# # Now we are left with all significant bvariables and hence this is our final model
orig_final_model <- orig_model_3
summary(orig_final_model)
# 
# Call:
#  glm(formula = Performance.Tag ~ No.of.trades.opened.in.last.12.months_woe + 
#        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
#        Avgas_CC_bin_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe, 
#      family = "binomial", data = final_train)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -0.4409  -0.3477  -0.2645  -0.1702   2.9129  

# Coefficients:
#  Estimate Std. Error  z value Pr(>|z|)    
#  (Intercept)                                                         -3.12471    0.02375 -131.583  < 2e-16 ***
#  No.of.trades.opened.in.last.12.months_woe                            0.33323    0.07695    4.331 1.49e-05 ***
#  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe  0.37472    0.08396    4.463 8.08e-06 ***
#  Avgas_CC_bin_woe                                                     0.44138    0.05674    7.778 7.34e-15 ***
#  No.of.times.30.DPD.or.worse.in.last.12.months_woe                    0.46752    0.05554    8.417  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# (Dispersion parameter for binomial family taken to be 1)

# Null deviance: 17100  on 48906  degrees of freedom
# Residual deviance: 16336  on 48902  degrees of freedom
# AIC: 16346

# Number of Fisher Scoring iterations: 6

################################################################################
# 2) over sampling
################################################################################
ov_model_1 <- glm(formula = Performance.Tag ~ ., family = "binomial", 
                   data = ov_final_train)
summary(ov_model_1)
 
# Performing StepAIC
ov_model_2 <- stepAIC(ov_model_1,direction = "both")
summary(ov_model_2)
data.frame(sort(vif(ov_model_2)))
 
# Removing variables greater than vif 2 and insignicant probabilities.
 
# Removing No.of.times.30.DPD.or.worse.in.last.6.months_woe
ov_model_3 <- glm(formula = Performance.Tag ~ No.of.trades.opened.in.last.12.months_woe + 
                  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                  No.of.PL.trades.opened.in.last.12.months_woe + Avgas_CC_bin_woe + 
                  Total.No.of.Trades_woe + No.of.PL.trades.opened.in.last.6.months_woe +
                  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                  No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                  No.of.trades.opened.in.last.6.months_woe + No.of.months.in.current.residence_woe + 
                  Income_grp_woe, family = "binomial", data = ov_final_train)
summary(ov_model_3)
data.frame(sort(vif(ov_model_3)))

# Removing No.of.trades.opened.in.last.12.months_woe
ov_model_4 <- glm(formula = Performance.Tag ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    No.of.PL.trades.opened.in.last.12.months_woe + Avgas_CC_bin_woe + 
                    Total.No.of.Trades_woe + No.of.PL.trades.opened.in.last.6.months_woe +
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                    No.of.trades.opened.in.last.6.months_woe + No.of.months.in.current.residence_woe + 
                    Income_grp_woe, family = "binomial", data = ov_final_train)
summary(ov_model_4)
data.frame(sort(vif(ov_model_4)))
 
# Removing No.of.trades.opened.in.last.6.months_woe
ov_model_5 <- glm(formula = Performance.Tag ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    No.of.PL.trades.opened.in.last.12.months_woe + Avgas_CC_bin_woe + 
                    Total.No.of.Trades_woe + No.of.PL.trades.opened.in.last.6.months_woe +
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                    No.of.months.in.current.residence_woe + 
                    Income_grp_woe, family = "binomial", data = ov_final_train)
summary(ov_model_5)
data.frame(sort(vif(ov_model_5)))

# Removing Total.No.of.Trades_woe
ov_model_6 <- glm(formula = Performance.Tag ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    No.of.PL.trades.opened.in.last.12.months_woe + Avgas_CC_bin_woe + 
                    No.of.PL.trades.opened.in.last.6.months_woe +
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                    No.of.months.in.current.residence_woe + 
                    Income_grp_woe, family = "binomial", data = ov_final_train)
summary(ov_model_6)
data.frame(sort(vif(ov_model_6)))

# Removing Income_grp_woe
ov_model_7 <- glm(formula = Performance.Tag ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    No.of.PL.trades.opened.in.last.12.months_woe + Avgas_CC_bin_woe + 
                    No.of.PL.trades.opened.in.last.6.months_woe +
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe + 
                    No.of.months.in.current.residence_woe, 
                    family = "binomial", data = ov_final_train)
summary(ov_model_7)
data.frame(sort(vif(ov_model_7)))

# Removing No.of.months.in.current.residence_woe
ov_model_8 <- glm(formula = Performance.Tag ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    No.of.PL.trades.opened.in.last.12.months_woe + Avgas_CC_bin_woe + 
                    No.of.PL.trades.opened.in.last.6.months_woe +
                    No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe, 
                    family = "binomial", data = ov_final_train)
summary(ov_model_8)
data.frame(sort(vif(ov_model_8)))

# Removing No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe 
ov_model_9 <- glm(formula = Performance.Tag ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    No.of.PL.trades.opened.in.last.12.months_woe + Avgas_CC_bin_woe + 
                    No.of.PL.trades.opened.in.last.6.months_woe +
                    No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe, 
                  family = "binomial", data = ov_final_train)
summary(ov_model_9)
data.frame(sort(vif(ov_model_9)))

# Now we are left with all significant variables and hence this is our final model
ov_final_model <- ov_model_9
summary(ov_final_model)
# 
# Call:
#  glm(formula = Performance.Tag ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
#        No.of.PL.trades.opened.in.last.12.months_woe + Avgas_CC_bin_woe + 
#        No.of.PL.trades.opened.in.last.6.months_woe + No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
#        No.of.times.90.DPD.or.worse.in.last.12.months_woe + No.of.times.30.DPD.or.worse.in.last.12.months_woe, 
#      family = "binomial", data = ov_final_train)

# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -1.5136  -1.0564  -0.7289   1.0516   1.7062  

# Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)                                                         -0.085871   0.007034 -12.208  < 2e-16 ***
#  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe  0.333195   0.028392  11.736  < 2e-16 ***
#  No.of.PL.trades.opened.in.last.12.months_woe                         0.215269   0.031904   6.747 1.51e-11 ***
#  Avgas_CC_bin_woe                                                     0.445598   0.017524  25.428  < 2e-16 ***
#  No.of.PL.trades.opened.in.last.6.months_woe                          0.107688   0.029025   3.710 0.000207 ***
#  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe   0.090253   0.026792   3.369 0.000755 ***
#  No.of.times.90.DPD.or.worse.in.last.12.months_woe                    0.173791   0.024920   6.974 3.08e-12 ***
#  No.of.times.30.DPD.or.worse.in.last.12.months_woe                    0.306594   0.024562  12.482  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#  (Dispersion parameter for binomial family taken to be 1)
#
#  Null deviance: 124615  on 89999  degrees of freedom
#  Residual deviance: 115617  on 89992  degrees of freedom
#  AIC: 115633
#
#  Number of Fisher Scoring iterations: 4

################################################################################
# 3)over sampling through SMOTE
################################################################################
ov_smote_model_1 <- glm(formula = Performance.Tag ~ ., family = "binomial", 
                         data = ov_smote_final_train)
summary(ov_smote_model_1)

# Performing StepAIC
ov_smote_model_2 <- stepAIC(ov_smote_model_1,direction = "both")
summary(ov_smote_model_2)
data.frame(sort(vif(ov_smote_model_2)))

# 
# Removing variables greater than vif 2 and insignicant probabilities.
 
# Removing No.of.times.30.DPD.or.worse.in.last.6.months_woe
ov_smote_model_3 <- glm(formula = Performance.Tag ~ No.of.trades.opened.in.last.12.months_woe + 
                        No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                        Avgas_CC_bin_woe + No.of.PL.trades.opened.in.last.6.months_woe +
                        No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                        No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.6.months_woe + 
                        No.of.trades.opened.in.last.6.months_woe + No.of.times.60.DPD.or.worse.in.last.12.months_woe, 
                        family = "binomial", data = ov_smote_final_train)
summary(ov_smote_model_3)
data.frame(sort(vif(ov_smote_model_3)))

# Removing No.of.times.60.DPD.or.worse.in.last.12.months_woe
ov_smote_model_4 <- glm(formula = Performance.Tag ~ No.of.trades.opened.in.last.12.months_woe + 
                          No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                          Avgas_CC_bin_woe + No.of.PL.trades.opened.in.last.6.months_woe +
                          No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                          No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.6.months_woe + 
                          No.of.trades.opened.in.last.6.months_woe, 
                          family = "binomial", data = ov_smote_final_train)
summary(ov_smote_model_4)
data.frame(sort(vif(ov_smote_model_4)))

# Removing No.of.trades.opened.in.last.12.months_woe
ov_smote_model_5 <- glm(formula = Performance.Tag ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                          Avgas_CC_bin_woe + No.of.PL.trades.opened.in.last.6.months_woe +
                          No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                          No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.6.months_woe + 
                          No.of.trades.opened.in.last.6.months_woe, family = "binomial", data = ov_smote_final_train)
summary(ov_smote_model_5)
data.frame(sort(vif(ov_smote_model_5)))

# Removing No.of.trades.opened.in.last.6.months_woe
ov_smote_model_6 <- glm(formula = Performance.Tag ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                          Avgas_CC_bin_woe + No.of.PL.trades.opened.in.last.6.months_woe +
                          No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
                          No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.6.months_woe, 
                          family = "binomial", data = ov_smote_final_train)
summary(ov_smote_model_6)
data.frame(sort(vif(ov_smote_model_6)))

# Now we are left with all significant variables and hence this is our final model
ov_smote_final_model <- ov_smote_model_6
summary(ov_smote_final_model)

# 
# Call:
#  glm(formula = Performance.Tag ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
#        Avgas_CC_bin_woe + No.of.PL.trades.opened.in.last.6.months_woe + 
#        No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe + 
#        No.of.times.30.DPD.or.worse.in.last.12.months_woe + No.of.times.60.DPD.or.worse.in.last.6.months_woe, 
#        family = "binomial", data = ov_smote_final_train)

# Deviance Residuals: 
#  Min       1Q      Median       3Q      Max  
#  -1.6120  -1.1020   0.7981   1.0177   1.6178  

# Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)                                                          0.08908    0.01015   8.776  < 2e-16 ***
#  No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe  0.29480    0.03822   7.713 1.22e-14 ***
#  Avgas_CC_bin_woe                                                     0.47952    0.02418  19.834  < 2e-16 ***
#  No.of.PL.trades.opened.in.last.6.months_woe                          0.32200    0.02872  11.210  < 2e-16 ***
#  No.of.Inquiries.in.last.6.months..excluding.home...auto.loans._woe   0.16722    0.03889   4.300 1.71e-05 ***
#  No.of.times.30.DPD.or.worse.in.last.12.months_woe                    0.18787    0.04790   3.923 8.76e-05 ***
#  No.of.times.60.DPD.or.worse.in.last.6.months_woe                     0.29587    0.04735   6.249 4.14e-10 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#  (Dispersion parameter for binomial family taken to be 1)

#  Null deviance: 59960  on 43322  degrees of freedom
#  Residual deviance: 55551  on 43316  degrees of freedom
#  AIC: 55565

#  Number of Fisher Scoring iterations: 4
# 

################################################################################
# 4) under sampling 
################################################################################
un_model_1 <- glm(formula = Performance.Tag~.,family = "binomial", 
                   data = un_final_train)
summary(un_model_1)
 
un_model_2 <- stepAIC(un_model_1,direction = "both")
summary(un_model_2)
data.frame(sort(vif(un_model_2)))
 
# Removing variables greater than vif 2 and insignicant probabilities.
 
# Removing No.of.trades.opened.in.last.12.months_woe
un_model_3 <- glm(formula = Performance.Tag ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                  Avgas_CC_bin_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe + 
                  No.of.PL.trades.opened.in.last.6.months_woe + No.of.times.90.DPD.or.worse.in.last.12.months_woe + 
                  No.of.months.in.current.residence_woe, family = "binomial", data = un_final_train)
summary(un_model_3)
data.frame(sort(vif(un_model_3)))

# Removing No.of.times.90.DPD.or.worse.in.last.12.months_woe 
un_model_4 <- glm(formula = Performance.Tag ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    Avgas_CC_bin_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe + 
                    No.of.PL.trades.opened.in.last.6.months_woe + No.of.months.in.current.residence_woe, 
                    family = "binomial", data = un_final_train)
summary(un_model_4)
data.frame(sort(vif(un_model_4)))

# Removing No.of.months.in.current.residence_woe
un_model_5 <- glm(formula = Performance.Tag ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
                    Avgas_CC_bin_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe + 
                    No.of.PL.trades.opened.in.last.6.months_woe, 
                    family = "binomial", data = un_final_train)
summary(un_model_5)
data.frame(sort(vif(un_model_5)))

# Now we are left with all significant variables and hence this is our final model
un_final_model <- un_model_5
summary(un_final_model)

# Call:
#  glm(formula = Performance.Tag ~ No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe + 
#        Avgas_CC_bin_woe + No.of.times.30.DPD.or.worse.in.last.6.months_woe + 
#        No.of.PL.trades.opened.in.last.6.months_woe, family = "binomial", 
#        data = un_final_train)

# Deviance Residuals: 
#  Min       1Q       Median       3Q      Max  
#  -1.5686  -1.0590  -0.7592   1.0567   1.6642  

# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)                                                         -0.01395    0.03281  -0.425 0.670604    
#   No.of.Inquiries.in.last.12.months..excluding.home...auto.loans._woe  0.34244    0.08852   3.868 0.000110 ***
#   Avgas_CC_bin_woe                                                     0.51522    0.07712   6.681 2.37e-11 ***
#   No.of.times.30.DPD.or.worse.in.last.6.months_woe                     0.51805    0.07837   6.611 3.83e-11 ***
#   No.of.PL.trades.opened.in.last.6.months_woe                          0.35261    0.09125   3.864 0.000112 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#  (Dispersion parameter for binomial family taken to be 1)
#
#  Null deviance: 5787.2  on 4174  degrees of freedom
#  Residual deviance: 5326.0  on 4170  degrees of freedom
#  AIC: 5336
#
#  Number of Fisher Scoring iterations: 4

################################################################
# Predicting all the models on test data
################################################################
table(test$Performance.Tag)
test <- final_test
test$Performance.Tag <- factor(ifelse(test$Performance.Tag=="good",0,1))

test_pred_orig <- predict(orig_final_model, type = "response", newdata = test)
summary(test_pred_orig)
test_pred_ov <- predict(ov_final_model, type = "response", newdata = test)
summary(test_pred_ov)
test_pred_ov_smote <- predict(ov_smote_final_model, type = "response", newdata = test)
summary(test_pred_ov_smote)
test_pred_un <- predict(un_final_model, type = "response", newdata = test)
summary(test_pred_un)

test_actual_default <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))

########################################################################################
# Finding out the optimal probalility cutoff and accuracy,sensitivity,specificity
########################################################################################
 
########################################################################################
# 1)Original data model
########################################################################################
test$prob <- test_pred_orig
 
perform_fn <- function(cutoff) 
{
   predicted_default <- factor(ifelse(test_pred_orig >= cutoff, "Yes", "No"))
   conf <- confusionMatrix(predicted_default, test_actual_default, positive = "Yes")
   acc <- conf$overall[1]
   sens <- conf$byClass[1]
   spec <- conf$byClass[2]
   out <- t(as.matrix(c(sens, spec, acc))) 
   colnames(out) <- c("sensitivity", "specificity", "accuracy")
   return(out)
}
 
s = seq(0.01786, 0.10572,length=100)
OUT = matrix(0,100,3)
 
for(i in 1:100)
{
   OUT[i,] = perform_fn(s[i])
} 
 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0.01,0.1,length=5),seq(0.01,0.1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.01, 0.25,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
 
cutoff <- min(s[which(abs(OUT[,1]-OUT[,2])<0.04)])
cutoff #0.0471
 
test_pred_default <- factor(ifelse(test$prob >= cutoff, "Yes", "No"))
test_actual_default <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))

table(test$Performance.Tag)
conf <- confusionMatrix(test_pred_default, test_actual_default, positive = "Yes")
 
conf$overall[1]  #-- Accuracy : 61.48%
conf$byClass[1]  #-- Sensistivity : 64.93%
conf$byClass[2]  # -- Specificity : 61.33%
 
#######################################################################################
# 2) over sampled data model
########################################################################################
test$prob <- test_pred_ov
 
perform_fn <- function(cutoff) 
{
   predicted_default <- factor(ifelse(test_pred_ov >= cutoff, "Yes", "No"))
   conf <- confusionMatrix(predicted_default, test_actual_default, positive = "Yes")
   acc <- conf$overall[1]
   sens <- conf$byClass[1]
   spec <- conf$byClass[2]
   out <- t(as.matrix(c(sens, spec, acc))) 
   colnames(out) <- c("sensitivity", "specificity", "accuracy")
   return(out)
}
 
s = seq(0.2114, 0.6932,length=100)
OUT = matrix(0,100,3)
 
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 
 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.25, 0.6,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
 
cutoff <- min(s[which(abs(OUT[,1]-OUT[,2])<0.04)])
cutoff #0.518
 
test_pred_default <- factor(ifelse(test$prob >= cutoff, "Yes", "No"))
test_actual_default <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))
 
conf <- confusionMatrix(test_pred_default, test_actual_default, positive = "Yes")
 
conf$overall[1]  #-- Accuracy : 62.10%
conf$byClass[1]  #-- Sensistivity : 63.80%
conf$byClass[2]  # -- Specificity : 62.03%
 
########################################################################################
# 3)over sampled through smote data model
########################################################################################
test$prob <- test_pred_ov_smote
 
perform_fn <- function(cutoff) 
{
   predicted_default <- factor(ifelse(test_pred_ov_smote >= cutoff, "Yes", "No"))
   conf <- confusionMatrix(predicted_default, test_actual_default, positive = "Yes")
   acc <- conf$overall[1]
   sens <- conf$byClass[1]
   spec <- conf$byClass[2]
   out <- t(as.matrix(c(sens, spec, acc))) 
   colnames(out) <- c("sensitivity", "specificity", "accuracy")
   return(out)
}
 
s = seq(0.2418, 0.7292,length=100) 
OUT = matrix(0,100,3)
 
for(i in 1:100)
{
   OUT[i,] = perform_fn(s[i])
} 
 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0.01,0.1,length=5),seq(0.01,0.1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.01, 0.5,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
 
cutoff <- min(s[which(abs(OUT[,1]-OUT[,2])<0.04)])
cutoff # 0.542
 
test_pred_default <- factor(ifelse(test$prob >= cutoff, "Yes", "No"))
test_actual_default <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))
 
conf <- confusionMatrix(test_pred_default, test_actual_default, positive = "Yes")
 
conf$overall[1]  #-- Accuracy : 61.12%
conf$byClass[1]  #-- Sensistivity : 64.25%
conf$byClass[2]  # -- Specificity : 60.98%
 
########################################################################################
# 4) under sampled data model
########################################################################################

test$prob <- test_pred_un
 
perform_fn <- function(cutoff) 
{
   predicted_default <- factor(ifelse(test_pred_un >= cutoff, "Yes", "No"))
   conf <- confusionMatrix(predicted_default, test_actual_default, positive = "Yes")
   acc <- conf$overall[1]
   sens <- conf$byClass[1]
   spec <- conf$byClass[2]
   out <- t(as.matrix(c(sens, spec, acc))) 
   colnames(out) <- c("sensitivity", "specificity", "accuracy")
   return(out)
}
 
s = seq(0.2305, 0.6766,length=100)
OUT = matrix(0,100,3)
 
for(i in 1:100)
{
   OUT[i,] = perform_fn(s[i])
} 
 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0.25, 0.6,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
 
cutoff <- min(s[which(abs(OUT[,1]-OUT[,2])<0.04)])
cutoff #0.532
 
test_pred_default <- factor(ifelse(test$prob >= cutoff, "Yes", "No"))
test_actual_default <- factor(ifelse(test$Performance.Tag==1,"Yes","No"))
 
conf <- confusionMatrix(test_pred_default, test_actual_default, positive = "Yes")
 
conf$overall[1]  #--     Accuracy : 61.52%
conf$byClass[1]  #-- Sensistivity : 63.91%
conf$byClass[2]  # -- Specificity : 61.41%
 
# The logistic model for under sampled produced the best results with
##    -- Accuracy : 61.52%
##-- Sensistivity : 63.91%
## -- Specificity : 61.41%

#################################################################
# Calculating KS statistic for the under sampled model

test_pred_default
test_actual_default
 
test_pred_default_1 <- ifelse(test_pred_default=="Yes",1,0)
test_actual_default_1 <- ifelse(test_actual_default=="Yes",1,0)
 
pred_object <- prediction(test_pred_default_1, test_actual_default_1)
 
performance_measures_test<- performance(pred_object, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
   (attr(performance_measures_test, "x.values")[[1]]) #0.252

ks_table_test 
 
#########################################################################
# Application scorecard for the test data using logistic regression model
#########################################################################
 
# Using the logistic model, the best model obtained in terms of specificity is the over sampled model

test_woe_logit <- final_test
 
# test_pred_un are the predicted values as per under sampled model which has best results
test_woe_logit$prob <- test_pred_un
 
test_woe_logit$prob_bad <- test_woe_logit$prob
test_woe_logit$prob_good <- 1 - test_woe_logit$prob_bad

test_woe_logit$odds_good_to_bad <- log(test_woe_logit$prob_good/test_woe_logit$prob_bad)
 
# # So going with the approach of "y = mx + c"
# 
# # Score = m(ln(odds)) + c
# 
# #400 = m * (ln(10)) + c
# #420 = m * (ln(20)) + c
# 
# #420 - 400 = m(ln(20) - ln(10)) 
# #20 = m (ln(10*2) - ln(10))
# #20 = m(ln(2) + ln(10) - ln(10))
# 
# #20 = m(ln(2))
# 
# #m = 20/ln(2)
# #---------------
# 
# #So base score  = 400 - 20/(ln(2)) * ln(10)
# 
# #=  400 - 20/log(2) * log(10)
# #= 333.56
# 
# # Hence score = (20/log(2))*(odds_good_to_bad) + 333.56
# 
# #20/log(2) #28.85
# 
# test_woe_logit$score <- 28.85 * (test_woe_logit$odds_good_to_bad) + 333.56
# 
# summary(test_woe_logit$score)
# 
# # Min.  1st Qu.  Median Mean    3rd Qu.  Max. 
# #312.3   321.2   340.9   339.7   363.7   368.3
# 
# #For this model cut off probability = 0.537
# 

# 
# #cut off score =329
# 
# 
# 
# 
# ###################################################################################
# # Application Scorecard for the rejected population using logistic regression model
# ###################################################################################

# Based on the logistic regression model , calculating the scores for the rejected population
rejected_woe_logit <- df_rejected
 
rejected_pred <- predict(un_final_model, type = "response", newdata = rejected_woe_logit)
 
rejected_woe_logit
rejected_woe_logit$prob <- rejected_pred

rejected_woe_logit$prob_bad <- rejected_woe_logit$prob
rejected_woe_logit$prob_good <- 1 - rejected_woe_logit$prob_bad
 
rejected_woe_logit$odds_good_to_bad <- log(rejected_woe_logit$prob_good/rejected_woe_logit$prob_bad)

# So going with the approach of "y = mx + c"
# 
# # Score = m(ln(odds)) + c
# 
# 400 = m * (ln(10)) + c
# 420 = m * (ln(20)) + c
# 
# 420 - 400 = m(ln(20) - ln(10)) 
# 20 = m (ln(10*2) - ln(10))
# 20 = m(ln(2) + ln(10) - ln(10))
# 
# 20 = m(ln(2))
# 
# m = 20/ln(2)
# ---------------
# 
# So base score  = 400 - 20/(ln(2)) * ln(10)
# 
# =  400 - 20/log(2) * log(10)
# = 333.56
# 
# Hence score = (20/log(2))*(odds_good_to_bad) + 333.56
# 
# 20/log(2) #28.85
# 

rejected_woe_logit$score <- 28.85 * (rejected_woe_logit$odds_good_to_bad) + 333.56
 
summary(rejected_woe_logit$score)
 
length(which(rejected_woe_logit$score < 349)) #1395 can be defaulters

# 30 can be good customers can be captured by  applying our model from the data set "performance tag" missing

# #Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# #311.9   312.7   316.6   317.1   316.6   356.5
 
###############################################################
#                  Decision Tree                              #
###############################################################
 
#########################################################
#   1) Model for oversampling data - Decision Tree      #
#########################################################
 
table(ov_final_train$Performance.Tag) 
# 
# #   0     1 
# # 46844 43156 
# 
# build tree model- default hyperparameters
oversample_tree_model <- rpart(Performance.Tag ~ .,             # formula
                              data = ov_final_train,            # training data
                              method = "class")                 # classification or regression

# display decision tree
prp(oversample_tree_model)
 
# make predictions on the test set
oversample_tree_predict <- predict(oversample_tree_model, test, type = "class")
 
# evaluate the results
conf <- confusionMatrix(oversample_tree_predict, test$Performance.Tag)
 
conf$overall[1]  # --      Accuracy : 59.02% 
conf$byClass[1]  # --  Sensistivity : 58.69%
conf$byClass[2]  # --    Specificity: 66.40%
 
#####################################################################
#   2) Model for oversampling data using SMOTE - Decision Tree      #
#####################################################################
 
table(ov_smote_final_train$Performance.Tag) 
 
#    0     1 
# 20630 22693 
 
# build tree model- default hyperparameters
oversample_smote_tree_model <- rpart(Performance.Tag ~ .,               # formula
                                     data = ov_smote_final_train,       # training data
                                      method = "class")                 # classification or regression
 
# display decision tree
prp(oversample_smote_tree_model)
 
# make predictions on the test set
oversample_smote_tree_predict <- predict(oversample_smote_tree_model, test, type = "class")
 
# evaluate the results
conf <- confusionMatrix(oversample_smote_tree_predict, test$Performance.Tag)
 
conf$overall[1]  # --      Accuracy : 68.61% 
conf$byClass[1]  # --  Sensistivity : 69.47%
conf$byClass[2]  # --    Specificity: 49.20%

# ################################################################
# #   3) Model for undersampling data using - Decision Tree      #
# ################################################################
 
table(un_final_train$Performance.Tag) 
 
#   0    1 
# 2112 2063 

# build tree model- default hyperparameters
undersample_tree_model <- rpart(Performance.Tag ~ .,          # formula
                                data = un_final_train,        # training data
                                method = "class")             # classification or regression

# display decision tree
prp(undersample_tree_model)
 
# make predictions on the test set
undersample_tree_predict <- predict(undersample_tree_model, test, type = "class")
 
# evaluate the results
conf <- confusionMatrix(undersample_tree_predict, test$Performance.Tag)
 
conf$overall[1]  # --      Accuracy : 59.02%
conf$byClass[1]  # --  Sensistivity : 58.69%
conf$byClass[2]  # --    Specificity: 66.40%
# 
# ###############################################################
#############################################################
#                                      Random forest
##################################################################

# Let us balance data using method "over". 
#sampled_rf_Train <- ovun.sample(Performance.Tag~.,data=final_train,method ="over",N=93688,seed =171)$data
levels(final_train$Performance.Tag)

#Similar to the above , the following code for under,both and ROSE have been run and the
#outputs are shown. The best model obtained is for "under" 

sampled_rf_Train <- ovun.sample(Performance.Tag~.,data=final_train,method ="under",N=4126,seed=78)$data

#sampled_rf_Train <- ovun.sample(Performance.Tag~.,data=final_train,method ="both",p=0.5,N=20960,seed=11)$data
#sampled_rf_Train <- ROSE(Performance.Tag~.,data=final_train,N=20960,seed=896)$data
#################################################################################################

# Shuffle the data
final_rf_train <- sampled_rf_Train[sample(nrow(sampled_rf_Train)), ]

# Build the random forest
set.seed(900)
data.rf <- randomForest(formula=Performance.Tag~., data=final_rf_train, proximity=FALSE,
                        ntree=500, mtry=c(4,5),node_size  = seq(2, 10, by = 2),
                        sampe_size = c(.55, .632, .70,.75, .80),replace=T,
                        do.trace=TRUE) #, na.action=na.omit


data.rf
importance(data.rf)


testPred <- predict(data.rf, newdata=final_test) # prediction in 0 and 1
#prediction in probability
testPred_prob <- predict(data.rf, newdata=final_test,type="prob",na.action=na.omit)
summary(testPred_prob[,1])
table(testPred, final_test$Performance.Tag)
final_test$Performance.Tag<-as.factor(final_test$Performance.Tag)
conf <- confusionMatrix(testPred, final_test$Performance.Tag)
conf

hist(treesize(data.rf))
varUsed(data.rf, by.tree=FALSE, count=TRUE)
varUsed(data.rf)
######################################################
# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 12274   313
# 1  7801   571
# 
# Accuracy : 0.6129          
# 95% CI : (0.6062, 0.6195)
# No Information Rate : 0.9578          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.051           
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.6114          
# Specificity : 0.6459          
# Pos Pred Value : 0.9751          
# Neg Pred Value : 0.0682          
# Prevalence : 0.9578          
# Detection Rate : 0.5856          
# Detection Prevalence : 0.6006          
# Balanced Accuracy : 0.6287          
# 
# 'Positive' Class : 0
#####################################################
# Method: Over

# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 13538   415
# 1  6537   469
# 
# Accuracy : 0.6683          
# 95% CI : (0.6619, 0.6747)
# No Information Rate : 0.9578          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0475          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.67437         
# Specificity : 0.53054         
# Pos Pred Value : 0.97026         
# Neg Pred Value : 0.06694         
# Prevalence : 0.95782         
# Detection Rate : 0.64593         
# Detection Prevalence : 0.66573         
# Balanced Accuracy : 0.60246         
# 
# 'Positive' Class : 0    
####################################################################

###################################################
#sampling with method = both


# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 12811   360
# 1  7264   524
# 
# Accuracy : 0.6362          
# 95% CI : (0.6297, 0.6428)
# No Information Rate : 0.9578          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0488          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.63816         
# Specificity : 0.59276         
# Pos Pred Value : 0.97267         
# Neg Pred Value : 0.06728         
# Prevalence : 0.95782         
# Detection Rate : 0.61124         
# Detection Prevalence : 0.62842         
# Balanced Accuracy : 0.61546         
# 
# 'Positive' Class : 0             
#####################################
#method =ROSE


# Confusion Matrix and Statistics
# 
# Reference
# Prediction     0     1
# 0 17219   629
# 1  2856   255
# 
# Accuracy : 0.8337          
# 95% CI : (0.8286, 0.8387)
# No Information Rate : 0.9578          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.0663          
# Mcnemar's Test P-Value : <2e-16          
# 
# Sensitivity : 0.85773         
# Specificity : 0.28846         
# Pos Pred Value : 0.96476         
# Neg Pred Value : 0.08197         
# Prevalence : 0.95782         
# Detection Rate : 0.82156         
# Detection Prevalence : 0.85157         
# Balanced Accuracy : 0.57310         
# 
# 'Positive' Class : 0  
###################################################################
table( testPred_prob[,2] > 0.5,final_test$Performance.Tag,useNA ="ifany")

varImpPlot(data.rf,color="darkblue",main = "Random_Forest_decreasing_variable importance")

varUsed(data.rf)
pred1=predict(data.rf,type = "prob")

library(ROCR)
perf = prediction(testPred_prob[,2],final_test$Performance.Tag)



#  True Positive and Negative Rate
pred2 = performance(perf, "tpr","fpr")

# Plot the ROC curve
plot(pred2,main="ROC Curve for Random Forest",col=3,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="blue")
#######################################################################################################################
#SCORECARD
######################################################################################################################
###################################################################
# Application scorecard for the test data using random forest model
###################################################################

test_woe_rf <- final_test

test_pred_rf <- predict(data.rf, newdata=final_test)
test_pred_rf_prob <- predict(data.rf, newdata=final_test , type = "prob") # prediction in 0 and 1

df <- as.data.frame(test_pred_rf_prob)

df$`1`<-ifelse(df$`1`==0,0.01,df$`1`) 
df$`1`<-ifelse(df$`1`==1,0.99,df$`1`)
test_woe_rf$prob_bad <-df$`1`
# index_1<-which(test_woe_rf$prob == 1)
# index_0<-which(test_woe_rf$prob == 0)
# x<-test_woe_rf$prob[-c(index_1,index_0)]
# 



test_woe_rf$prob_good <- 1 - test_woe_rf$prob_bad



test_woe_rf$odds_good_to_bad <- log(test_woe_rf$prob_good/test_woe_rf$prob_bad)

test_woe_rf$score <- 28.85 * (test_woe_rf$odds_good_to_bad) + 333.56
summary(test_woe_rf$score)


# Cutoff score respective to 0.5 proabaility

unique(test_woe_rf$score[which(test_woe_rf$prob_bad == 0.5)]) #333.56

length(which(test_woe_rf$score >= 333.56)) #12358
length(which(test_woe_rf$score < 333.56)) # 8601


summary(test_woe_rf$score)



#############################################################################
# Application scorecard for the test and rejected population using Random Forest Model
#############################################################################


#######################################################

colSums(is.na(df_rejected))
rejected_woe_rf <- df_rejected


rejected_pred_rf <- predict(data.rf, newdata=rejected_woe_rf) # prediction in 0 and 1

#prediction in probability

rejected_pred_rf_prob <- predict(data.rf, newdata=rejected_woe_rf,type="prob")
rejected_pred_rf_prob

df <- as.data.frame(rejected_pred_rf_prob)

rejected_woe_rf$prob <- df$`1`
rejected_woe_rf$prob<-ifelse(rejected_woe_rf$prob==0,0.01,rejected_woe_rf$prob)
rejected_woe_rf$prob<-ifelse(rejected_woe_rf$prob==1,0.99,rejected_woe_rf$prob)



rejected_woe_rf$prob_bad <- rejected_woe_rf$prob
rejected_woe_rf$prob_good <- 1 - rejected_woe_rf$prob_bad

rejected_woe_rf$odds_good_to_bad <- log(rejected_woe_rf$prob_good/rejected_woe_rf$prob_bad)

# So going with the approach of "y = mx + c"

# Score = m(ln(odds)) + c

#400 = m * (ln(10)) + c
#420 = m * (ln(20)) + c

#420 - 400 = m(ln(20) - ln(10)) 
#20 = m (ln(10*2) - ln(10))
#20 = m(ln(2) + ln(10) - ln(10))

#20 = m(ln(2))

#m = 20/ln(2)
#---------------

#So base score  = 400 - 20/(ln(2)) * ln(10)

#=  400 - 20/log(2) * log(10)
#= 333.56

# Hence score = (20/log(2))*(odds_good_to_bad) + 333.56

#20/log(2) #28.85

rejected_woe_rf$score <- 28.85 * (rejected_woe_rf$odds_good_to_bad) + 333.56

summary(rejected_woe_rf$score)


length(which(rejected_woe_rf$score >= 333.56))#71
#Thus we are able to capture 71 good customers from the set of applicants whose performance tag is missing
###############################################  END ############################################################






