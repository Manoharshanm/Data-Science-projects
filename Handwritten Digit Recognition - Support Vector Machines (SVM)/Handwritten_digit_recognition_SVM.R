#######################################################################
#   Case study - Support Vector Machines (SVM)                        #
#   Name: Manohar Shanmugasundaram                                    #
#   Roll ID: DDA1730068                                               #
#######################################################################

# import the required libraries
library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(doParallel) 

# Load the training data
train <- read.csv("mnist_train.csv", header = F, stringsAsFactors = FALSE)

head(train,1)

# Load the test data
test <- read.csv("mnist_test.csv", header = F, stringsAsFactors = FALSE)

head(test,1)

# Since the training and test data sets are provided as separate file, so combining these 
# for data cleaning
master <- rbind(train,test)

###############################################
#   Data Preparation and Cleaning             #
###############################################

# 1. Check wether there are any 'NA' values in the code
sum(is.na(master))

# 2. Replace the empty values of "charcter" type columns to NA (If there are any)
replace_emp_vals_to_NA <- function(df) {
  df <- df %>% mutate_if(is.character, function(x) gsub("^$|^ $", NA, x)) }

master <- replace_emp_vals_to_NA(master)

# recheck 'NA' values
sum(is.na(master))

# 3. Filter the columns which has more near zero values
master_zero_indices <- nearZeroVar(master, freqCut = 95/5, uniqueCut = 5)
master <- master[,-(master_zero_indices)]

# 4. Rename the dependent variable with the label as 'number'
names(master)[1]<-paste("number")

###################################
# Exploratory Data Analysis (EDA) #
###################################

# Bar plot to check all number have enough records to train and test the model
ggplot(data=master,aes(as.factor(master$number))) +
  geom_bar(color=I('black'),fill=I('#56B4E9')) +
  ggtitle("Bar plot") + labs(x = "number") + labs(y = "total record count") +
  geom_text(stat='count',aes(label=..count..),vjust=-1,size=3) 

# Insight - All the numbers 0 to 9 seems to have a fair equal amount of records to train and test.

############################################################################################################
# Split the train and test records from the master data frame, since the data cleanup and EDA are complete #
############################################################################################################

# Making our target class to factor
master$number <-factor(master$number)

# Train dataset will be from row number 1 till 60000
train <- master[1:60000,]

# Test dataset will be from row number 60001 till 70000
test <- master[60001:70000,]

#################################
##    Construct the Model      ##
#################################

# Due to large training MNIST dataset it will take huge computation time to build the model with the entire
# training dataset. Hence let's use a sample of 20% training data to build the model

# Select the sample of the train data set
set.seed(100)

# Select 20% of the data from the train data for model building
train.indices = sample(1:nrow(train), 0.2*nrow(train))
train_model_dt = train[train.indices, ]

#################################
##   Model using Linear Kernel ##
#################################

# Using Linear Kernel
Model_linear <- ksvm(number~ ., data = train_model_dt, scale = FALSE, kernel = "vanilladot")

# predict the model with the test data
Eval_linear<- predict(Model_linear, test)

# confusion matrix - Linear Kernel
confusionMatrix(Eval_linear,test$number)

# Accuracy : 0.8924
#                        Class:0  Class:1  Class:2  Class:3  Class:4  Class:5  Class:6  Class:7  Class:8  Class:9
# Sensitivity            0.9622   0.9850   0.8692   0.8950   0.9440   0.8251   0.8956   0.9076   0.8018   0.8196
# Specificity            0.9899   0.9913   0.9844   0.9812   0.9835   0.9855   0.9937   0.9912   0.9890   0.9908
# Pos Pred Value         0.9120   0.9356   0.8650   0.8425   0.8615   0.8479   0.9377   0.9219   0.8875   0.9088
# Neg Pred Value         0.9959   0.9981   0.9849   0.9881   0.9938   0.9829   0.9890   0.9894   0.9788   0.9800
# Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
# Detection Rate         0.0943   0.1118   0.0897   0.0904   0.0927   0.0736   0.0858   0.0933   0.0781   0.0827
# Detection Prevalence   0.1034   0.1195   0.1037   0.1073   0.1076   0.0868   0.0915   0.1012   0.0880   0.0910
# Balanced Accuracy      0.9761   0.9882   0.9268   0.9381   0.9637   0.9053   0.9447   0.9494   0.8954   0.9052

#################################
##   Model using RBF Kernel    ##
#################################

# RBF (Radial basis function) Kernel model
Model_RBF <- ksvm(number~ ., data = train_model_dt, scale = FALSE, kernel = "rbfdot")

# predict using the RBF model
Eval_RBF<- predict(Model_RBF, test)

# confusion matrix - RBF Kernel
confusionMatrix(Eval_RBF,test$number)

# Accuracy : 0.959          

# Statistics by Class:
#                       Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            0.9847   0.9894   0.9506   0.9644   0.9654   0.9439   0.9708   0.9416   0.9405   0.9346
# Specificity            0.9957   0.9973   0.9951   0.9944   0.9931   0.9952   0.9966   0.9959   0.9965   0.9948
# Pos Pred Value         0.9612   0.9791   0.9571   0.9512   0.9386   0.9503   0.9677   0.9632   0.9662   0.9525
# Neg Pred Value         0.9983   0.9986   0.9943   0.9960   0.9962   0.9945   0.9969   0.9933   0.9936   0.9927
# Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
# Detection Rate         0.0965   0.1123   0.0981   0.0974   0.0948   0.0842   0.0930   0.0968   0.0916   0.0943
# Detection Prevalence   0.1004   0.1147   0.1025   0.1024   0.1010   0.0886   0.0961   0.1005   0.0948   0.0990
# Balanced Accuracy      0.9902   0.9934   0.9728   0.9794   0.9793   0.9696   0.9837   0.9688   0.9685   0.9647

# Conclusion - From above 2 models, it is definetly the model created in RDF kernel provided a better prediction accuracy
#              of 96% compared to the model with the linear kernel 90%. In the next step will do the hyper parameter 
#              tuning and cross validation to indentify correct sigma and c value.

###############################################
#  Hyperparameter tuning and Cross Validation #
###############################################

# Use the train control function to do the cross validation and the number of folds as 6
trainControl <- trainControl(method="cv", number=6)

# Implies our Evaluation metric is Accuracy.
metric <- "Accuracy"

# Expand.grid functions takes set of hyperparameters, that we shall pass to our model.

set.seed(7)
grid <- expand.grid(.sigma=seq(0.01, 0.03, by=0.01), .C=seq(1, 3, by=1))

########################
#  Parallel processing #
########################

# For Cross validation function to work, it requires high processing power. Hence parallel proceessing 
# will be required to enable the computation to be faster.

# determine the number of cores and run with number of cores -1
no_cores <- detectCores() - 1  

# register for parallel processing
registerDoParallel(cores=no_cores)

cl <- makeCluster(no_cores, type="FORK")  

# train function takes Target ~ Prediction, Data, Method = Algorithm
# Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(number~., data=train_model_dt, method="svmRadial", metric=metric, 
                 tuneGrid=grid, trControl=trainControl)

# stop the clusters
stopCluster(cl)

# print the model
print(fit.svm)

# plot the model
plot(fit.svm)

##############################
# Result of Cross validation #
##############################
# Support Vector Machines with Radial Basis Function Kernel 

# 12000 samples
# 250 predictor
# 10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 

# No pre-processing
# Resampling: Cross-Validated (6 fold) 
# Summary of sample sizes: 9998, 10001, 10000, 10000, 10001, 10000, ... 
# Resampling results across tuning parameters:
  
#   sigma  C  Accuracy   Kappa    
#   0.01   1  0.9668330  0.9631411
#   0.01   2  0.9679996  0.9644376
#   0.01   3  0.9681663  0.9646228
#   0.02   1  0.9415836  0.9350844
#   0.02   2  0.9425840  0.9361958
#   0.02   3  0.9425840  0.9361958
#   0.03   1  0.8521653  0.8357058
#   0.03   2  0.8600826  0.8445043
#   0.03   3  0.8600826  0.8445043

# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were sigma = 0.01 and C = 3.

###############################################################################
# Evaluate the test data with the best fit model produced in the above step   #
###############################################################################

# Validating the model results on test data
eval_cross_val_model<- predict(fit.svm, test)

# confusion matrix
confusionMatrix(eval_cross_val_model, test$number)

# Accuracy : 0.97

#                       Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity            0.9898   0.9894   0.9690   0.9703   0.9725   0.9619   0.9770   0.9562   0.9630   0.9485
# Specificity            0.9971   0.9982   0.9954   0.9963   0.9966   0.9970   0.9978   0.9962   0.9957   0.9963
# Pos Pred Value         0.9739   0.9860   0.9606   0.9674   0.9686   0.9695   0.9791   0.9666   0.9601   0.9667
# Neg Pred Value         0.9989   0.9986   0.9964   0.9967   0.9970   0.9963   0.9976   0.9950   0.9960   0.9942
# Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
# Detection Rate         0.0970   0.1123   0.1000   0.0980   0.0955   0.0858   0.0936   0.0983   0.0938   0.0957
# Detection Prevalence   0.0996   0.1139   0.1041   0.1013   0.0986   0.0885   0.0956   0.1017   0.0977   0.0990
# Balanced Accuracy      0.9935   0.9938   0.9822   0.9833   0.9845   0.9795   0.9874   0.9762   0.9794   0.9724

##############
# Conculsion #
##############
# Following are the analysis and findings. Below are the models and the correspoding accuracy on the test data
# Linear Kernel model - Accuracy - 0.8924
# RBF Kernel model    - Accuracy - 0.959
# Hyperparameter turning and Cross validation - Accuracy - 0.9681 (using sigma '0.01' & C '3') on the training data 
# and this model provided the Accuracy of '0.97' on the test data. Hence it is clear that after hyperturning 
# got the best fit model

#####################################  END  #################################