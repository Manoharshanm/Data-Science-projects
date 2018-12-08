                      ###################################################
                      #         EDA Case Study Assignment               #
                      ###################################################
# Group members 
# D Mruthyunjaya Kumar (Facilitator) - Roll Number - DDA1730298
# Dharmanandana Reddy Pothula
# Ashwin Suresh
# Manohar Shanmugasundaram

# Import the required libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(gridExtra)
library(caret)
library(PerformanceAnalytics)

# Read the data from the data file provided for the case study
loan <- read.csv("loan.csv", stringsAsFactors = FALSE)

###############################
# Data Cleaning
###############################

# 1. Select only the required fields into a new data frame
# After analysis, we identified the following field neccessary fields to be considered for this case study
# and all the other fields are ignored.

# The logic for the column rejection are below.
# 1. Ignored all the columns only have 'NA' values

#------------------------------------------------------------------------------
# Treatment of Na and selection of columns #
#------------------------------------------------------------------------------

# check the distribution of NA
barplot(colMeans(is.na(loan)))

# Remove columns with NA more than 20%
dat1 <- loan[, colMeans(is.na(loan)) <= .2]
dim(dat1)
barplot(colMeans(is.na(dat1)))

# Remove Zero and Near Zero-Variance columns as they cannot impact the other variables
nzv <- nearZeroVar(dat1)
dat2 <- dat1[, -nzv]
dim(dat2)
barplot(colMeans(is.na(dat2))) 

# 2. Ignored all the columns which are related to customer payments (since these details will not help for this analysis)
# 3. Ignored the other fields like zip_code, emp_title, URL, etc, as these not related to this analysis 

# Selecting the subset of records after removing the above mentioned variables
loan_dt <- subset(dat2, select = c(loan_amnt, term, int_rate, grade, sub_grade, emp_length,
                         home_ownership, annual_inc, verification_status, loan_status, dti, pub_rec, 
                         total_acc, open_acc, purpose, installment, revol_util, revol_bal))

barplot(colMeans(is.na(loan_dt))) # NA are completely removed
dim(loan_dt)

# 2. Check for duplicate records. 
nrow(unique(loan_dt))

### Result - No duplicates records found, since the unique record count matches the total count

###########################################
#   Outlier Identification and removal    #
###########################################

# box plot to check for outliers
loan_dt %>%
  filter(!is.na(emp_length)) %>%
  ggplot(aes(x=emp_length, y=annual_inc)) + geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Remove the outlier from the dataset
loan_dt<-loan_dt[!(loan_dt$annual_inc >= 1000000.0 & loan_dt$annual_inc <= 6000000.0),]

# box plot after removing the outliers
loan_dt %>%
  filter(!is.na(emp_length)) %>%
  ggplot(aes(x=emp_length, y=annual_inc)) + geom_boxplot() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

###########################################
# Univarite & Derived Metrics Analysis    #
###########################################

###########################
# Correlation Analysis    #
###########################

#-----------------------------------------------------------------------------------
#filter data for defaulters
defaulters <- loan_dt %>% filter(loan_dt$loan_status == "Charged Off")
numeric_data<-defaulters[sapply(defaulters,is.numeric)]
chart.Correlation(numeric_data, histogram=TRUE, pch=23,main="corr_Hist_scatter_density for Defaulters")

#correlation matrix    
corr_df<-as.data.frame(cor(numeric_data)) 
print(corr_df)
View(corr_df)

# Data cleaning - Remove additional characters texts from the below variables for numeric analysis and grouping

loan_dt$int_rate = as.numeric(gsub("\\%", "", loan_dt$int_rate))
loan_dt$term = as.numeric(gsub("\\months", "", loan_dt$term))
loan_dt$revol_util = as.numeric(gsub("\\%", "", loan_dt$revol_util))

#######################################
#         Derived Variables           #
#######################################
# 1. Derving a new column for default or not, based on the loan status. 
# This field will be useful for applying correlation
default_flag <- function(loan_status){
  if(loan_status=="Charged Off"){
    out = 1
  }else{
    out = 0
  }
  return(out)
}

# invoke the function using lapply
loan_dt$default <- lapply(loan_dt$loan_status,default_flag)

# convert the field to numeric
loan_dt$default <- as.numeric(loan_dt$default)

##################################################
# 2. Creating a bin as below for the interest rate
#     Group       Interest rate
#     -----       -------------
#     Low         int_rate < 10
#     Medium      int_rate >=10 and < 15
#     High        int_rate >= 15

# Initialise the variable
int_rate_grp <- function(int_rate){
  if (int_rate < 10){
    out = "Low"
  }else if(int_rate >= 10 &  int_rate < 15){
    out = "Medium"
  }else if(int_rate >= 15){
    out = "High"
  }
  return(out)
}

# invoke the function using lapply
loan_dt$int_rate_group <- lapply(loan_dt$int_rate,int_rate_grp)

loan_dt$int_rate_group <- as.character(loan_dt$int_rate_group)

##############################################
# 3. Create a bin based on the customer income
#     Income group        Annual income
#     -------------       --------------
#     <=25 thousand       <= 25000
#     25 to 50 thousand   > 25000 and <= 50000
#     50 to 75 thousand   > 50000 and <= 75000
#     75 to 1 million     > 75000 and <= 100000
#     1 to 2 million      > 100000 and <= 200000
#     2 to 10 million     > 200000 and <= 1000000
#     10 to 60 million    > 1000000 and <= 6000000

loan_dt$annual_inc_grp <- cut((loan_dt$annual_inc), 
                          breaks=c(0,25000,50000,75000,100000,200000,1000000,6000000), 
                          labels=c("<=25 thousand","25 to 50 thousand","50 to 75 thousand","75 to 1 million","1 to 2 million",
                                    "2 to 10 million","10 to 60 million"),include.lowest=T, na.rm = TRUE)

##############################################
# 4. Create a bin based on the installment
#     Installment Group        Installment
#     -----------------       --------------
#     <=200                     <= 200
#     200 to 500                > 200 and <= 500
#     500 to 750                > 500 and <= 750
#     750 to 1000               > 750 and <= 1000
#     1000 to 1500              > 1000 and <= 1500
loan_dt$installment_grp <- cut((loan_dt$installment), 
                       breaks=c(0,200,500,750,1000,1500), 
                       labels=c("<=200","200 to 500","500 to 750","750 to 1000","1000 to 1500"),
                       include.lowest=T, na.rm = TRUE)

##############################################
# 5. Create a bin based on the dti
#     dti group               dti
#     -----------------       --------------
#     <=5                     <= 5
#     5 to 10                 > 5 and <= 10
#     10 to 15                > 10 and <= 15
#     15 to 20                > 15 and <= 20
#     20 to 25                > 20 and <= 25
#     25 to 30                > 25 and <= 30
loan_dt$dti_grp <- cut((loan_dt$dti), 
                       breaks=c(0,5,10,15,20,25,30), 
                       labels=c("<=5","5 to 10","10 to 15","15 to 20","20 to 25","25 to 30"),
                       include.lowest=T, na.rm = TRUE)

##############################################
# 6. Create a bin based on the revol_util
#     revol util grp          revol_util
#     -----------------       --------------
#     <=25                    <= 25
#     25 to 50                > 25 and <= 50
#     50 to 75                > 50 and <= 75
#     55 to 100               > 75 and <= 100
loan_dt$revol_util_grp <- cut((loan_dt$revol_util), 
                              breaks=c(0,25,50,75,100), 
                              labels=c("<=25","25 to 50","50 to 75","75 to 100"),
                              include.lowest=T, na.rm = TRUE)

#######################################
#         Univariate Analysis         #
#######################################

############### Univariate Analysis on CATEGORICAL VARIABLES ##########
############### 1. BAR plor for term  ####################################
ggplot(data=loan_dt,aes(as.factor(term))) +
  geom_bar(color=I('black'),fill=I('#56B4E9')) +
  ggtitle("Bar Plot for Term") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=3)

# Insight from plot - The more loans are with 36 months term

############### 2. BAR plot for home ownership ####################################
ggplot(data=loan_dt,aes(home_ownership)) + 
  geom_bar(color=I('black'),fill=I('#56B4E9')) +
  ggtitle("Bar Plot for Home Ownership") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=3) + labs(x = "home ownership") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Insight from plot - The major home ownership is with 'Mortgage' & 'Rent' almost 90%

############### 3. BAR plot for grade ####################################
ggplot(data=loan_dt,aes(grade)) + 
  geom_bar(color=I('black'),fill=I('#56B4E9')) + 
  ggtitle("Bar Plot for Grade") + geom_text(stat='count',aes(label=..count..),vjust=-1)

# Insight from plot - The Grades 'A', 'B', 'C' and 'D' have more loans

############### 4. BAR plot for Employment Length ####################################
ggplot(data=loan_dt,aes(emp_length)) + geom_bar(color=I('black'),fill=I('#56B4E9'))+
  ggtitle("Bar Plot for Employment Length") + geom_text(stat='count',aes(label=..count..),vjust=-1,size=3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Insight from plot - More laons are taken by employees with 0 to 5 years and 10+ years of experience

############### BAR plot for Purpose ####################################
ggplot(data=loan_dt,aes(purpose))+geom_bar(color=I('black'),fill=I('#56B4E9'))+
  ggtitle("Bar Plot for Purpose")+geom_text(stat='count',aes(label=..count..),vjust=-1,size=3) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Insight from plot - The loan purposes of 'debt_consolidation', 'credit_card', 'Other' and 'small_business' 
# have move loans.

############### BAR plot for Verificaiton status ####################################
ggplot(data=loan_dt,aes(verification_status)) + geom_bar(color=I('black'),fill=I('#56B4E9')) +
  ggtitle("Bar Plot for Purpose") + geom_text(stat='count',aes(label=..count..),vjust=-1)

# Insight from plot - The verification status 'Not Verified' have more loans but this is not a significant number

##########################################################################
#   BIVARIATE ANALYSIS - CATEGORICAL & CONTINUOUS VARIABLES           #
##########################################################################

# 1. BAR plot for Annual Income group ~ loan default
loan_dt %>%
  ggplot(aes(x=annual_inc_grp, fill=as.factor(default))) + 
  geom_bar(position = 'dodge') +
  labs(x = "Annual Income Group") + labs(y = "Loan Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name="Loan Default", breaks=c("0", "1"), labels=c("Non defaulter", "Defaulter"))

# Insight from plot - The more loans are taken by customers with an annual income of 25 thousand and 75 thousand
# and hence the number defaulters are more in this income group.

# 2. BAR plot for grade ~ loan default
ggplot(loan_dt, aes(x=grade, fill=as.factor(default))) + geom_bar(position = 'dodge')  +
    scale_fill_discrete(name="Loan Default",breaks=c("0", "1"),labels=c("Non defaulter", "Defaulter"))

# Insight from plot - The more number of defaulters are with grades 'B', 'C' and 'D'.

# 3. BAR plot for sub grade ~ loan default
ggplot(loan_dt, aes(x=sub_grade, fill=as.factor(default))) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_bar(position = 'dodge') +
    scale_fill_discrete(name="Loan Default",breaks=c("0", "1"),labels=c("Non defaulter", "Defaulter"))

# Insight from plot - This confirms the sub grades within grade 'B', 'C' and 'D' have more dafaulters

#######################################################################################
### Note: Here on all the plots and analysis will be done on the defaulter subset,  ### 
### where default == 1 (loan_status = 'Charged Off')                                ### 
#######################################################################################

# 4. BAR plot for Grade ~ Interest Rate Group for the defaulters
loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=grade,fill=as.factor(int_rate_group))) + geom_bar() +
  ggtitle("Grade ~ Interest Rate Group") + scale_fill_discrete(name="Interest Rate Group")

# Insight from plot - Interest Rate Group 'High' and 'Low' are having defaulter loans in grades 'B', 'C' and 'D'.
# Hence, these 2 variables are definitely driver variables for defaulter indentification.

# 5. BAR plot for Purpose ~ Interest Rate Group for the defaulters
loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=purpose,fill=int_rate_group)) + 
    geom_bar(stat="count",position = "dodge",col="black") +
    ggtitle("Purpose ~ Interest Rate Group") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    scale_fill_discrete(name="Interest Rate Group")

# Insight from plot - The loan purposes of 'debt_consolidation', 'credit_card', 'Other' and 'small_business' 
# have move loans. Hence this variable with these values are a driver for default identification

# 6. BAR plot for Home Ownership ~ Interest Rate Group for the defaulters
loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=home_ownership,fill=int_rate_group)) +
  geom_bar(stat="count",position = "dodge",col="black") +
  ggtitle("Home Ownership ~ Interest Rate Group") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Insight from plot - The major home ownership is with 'Mortgage' & 'Rent' almost 90%
  
# 7. BAR plot for Loan Amount ~ Interest Rate Group for the defaulters
loan_dt %>%
  filter(default == 1) %>%
  ggplot() + geom_bar(aes(x = loan_amnt, fill = int_rate_group), stat = "bin", position = "stack", bins = 30) +
    ggtitle("Loan Amount  ~ Interest Rate Group") + scale_fill_discrete(name="Interest Rate Group")

# Insight from plot - Loan amount from 100 to 25000 have more defaulters
  
# 8. BAR plot for Monthly Installments ~ Home Ownership for the defaulters
loan_dt %>%
  filter(default == 1) %>%
  ggplot() + geom_bar(aes(x = installment, fill = home_ownership), stat = "bin", position = "stack", bins = 30) +
    ggtitle("Installments  ~ Home Ownership") + scale_fill_discrete(name="Home Ownership")

# Insight from plot - The major home ownership is with 'Mortgage' & 'Rent' and installments between 100 and 500  
# have more defaulters

# 10 Bar plot for pub_rec ~ default 
ggplot(loan_dt, aes(x=pub_rec, fill=as.factor(default))) + geom_bar(position = 'dodge')  

# Insight from plot - Customer with zero public records are having more defaults. This implies bank already rejects 
# customer with any public record history for loan application

#########################################
#   Analysis for driver variables       #
#########################################

loan_dt %>%
  filter(default == 1 & emp_length != 'n/a') %>%
  ggplot(aes(x=annual_inc_grp, fill=as.factor(emp_length))) + 
  geom_bar(position = 'dodge') +
  ggtitle("Annual Income Group ~ Employee length for defaulters") +
  labs(x = "Annual Income Group") + labs(y = "Default Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name="Employee Length")

# Insight from plot - Annual income  25 to 50 thousand & 50 to 70 thousand are the major driver for default.
# Additionally employee length of 0 to 5 and 10+ are have more default records

loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=purpose,fill=int_rate_group)) + 
  geom_bar(stat="count",position = "dodge") +
  ggtitle("Purpose ~ Interest Rate Group for defaulters") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_fill_discrete(name="Interest Rate Group")

# Insight from plot - The loan purposes of 'debt_consolidation', 'credit_card', 'Other' and 'small_business' 
# have move loans with interest rate 'Medium' and 'High' bins. Hence these variables can be considered as driver variables 
# for defaulter identification

loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=grade, fill=grade)) + 
  geom_bar() +
  ggtitle("Grade analysis for defaulters") 

# Insight from plot - The Grades B', 'C' and 'D' have more defaulters. Hence this can considered for 
# defaulter identification

loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=sub_grade, fill=sub_grade)) + 
  geom_bar() +
  ggtitle("Grade analysis for defaulters") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Insight from plot - The sub grades of 'B', 'C' and 'D' have more defaulters.

loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=emp_length)) + 
  geom_bar(fill='blue')  +
  ggtitle("Employee length for defaulters") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Insight from plot - Employee length of 0 to 5 and 10+ are have more default records

loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=annual_inc_grp,fill=installment_grp)) + 
  geom_bar(position = 'dodge') +
  ggtitle("Installment ~ Annual income for defaulters") +
  labs(x = "Annual Income Group") + 
  scale_fill_discrete(name="Installment Group") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Insight from plot - Installment with 200 to 750 have more defaulters. Hence this variable can be considered
# for the defaulter indentiifcation.

loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=annual_inc_grp,fill=as.factor(term))) + 
  geom_bar(position = 'dodge') + scale_fill_discrete(name="Term") + 
  ggtitle("Annual income ~ term for defaulters") + labs(x = "Annual Income Group") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Insight from plot - Term 36 has more defaulters for annual income 25 to 50k and near equal for 50 to 75k for 36 
# and 60 months term

# Bar plot for Home Ownership  ~ Interest Rate Group for the defaulters
loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=home_ownership,fill=int_rate_group)) + geom_bar(stat="count",position = "dodge",col="black") +
  ggtitle("Home Ownership ~ Interest Rate Group")

# Bar plot for Home Ownership  ~ Installments for the defaulters
loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=installment,fill=home_ownership)) + geom_bar(stat = "bin",bins=30, position = "stack") +
  ggtitle("Installment ~ Home Ownership ") + scale_fill_discrete(name="Home Ownership")

###########################################
# Plots for final analysis and conclusion
##########################################

plot1 <- loan_dt %>%
  filter(default == 1 & annual_inc >= 0 & annual_inc <= 100000) %>%
  ggplot(aes(x=annual_inc)) + 
  ggtitle("Annual income") + labs(x = "Annual Income") + 
  geom_histogram(fill='brown',bins = 30)

plot2 <- loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=int_rate)) + 
  ggtitle("Interest rate") + labs(x = "Interest rate") + 
  geom_histogram(fill='brown',bins = 20)

plot3 <- loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=installment)) + 
  ggtitle("Installement") + labs(x = "Installment") + 
  geom_histogram(fill='brown',bins = 70)

plot4 <- loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=grade, fill=grade)) + 
  geom_bar() +
  ggtitle("Grade analysis") 

plot5 <- loan_dt %>%
  filter(default == 1 & emp_length != 'n/a') %>%
  ggplot(aes(x=emp_length)) + 
  geom_bar(fill='blue') + labs(x = "Employee Length") +
  ggtitle("Employee Length") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

plot6 <- loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=purpose)) + 
  geom_bar(fill='blue') + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Purpose") 

plot7 <- loan_dt %>%
  filter(default == 1 & (!is.na(revol_util_grp))) %>%
  ggplot(aes(x=purpose,fill=revol_util_grp)) + 
  geom_bar() +  scale_fill_discrete(name="Revolving Util Group") + 
  ggtitle("Purpose ~ Revolving Util") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

plot8 <- loan_dt %>%
  filter(default == 1) %>%
  ggplot(aes(x=home_ownership,fill=int_rate_group)) + geom_bar(stat="count",position = "dodge",col="black") +
  ggtitle("Home Ownership ~ Interest Rate Group")

# Display all the plots in the single page
grid.arrange(plot1,plot2,plot3,plot4,plot5,plot6,plot7,plot8, ncol = 3, nrow=3, top = "EDA analysis for defaulters")

######################### END ####################


