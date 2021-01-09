##### Packages
install.packages('tidyverse')
library('tidyverse')
library('ggplot2')
library(dplyr)
install.packages('ggpubr')
library('ggpubr')
library('skimr')
library(DMwR)
##############################

##### Loading Data
#Using read_csv faster rather than read.csv when large data 
train_trans <- read_csv('data/train_transaction.csv')
train_id <- read_csv('data/train_identity.csv')
test_trans <- read_csv('data/test_transaction.csv')
test_id <- read_csv('data/test_identity.csv')

##### memory problem :If the memory error occurs:cannot allocate vector of size n MB (N for any numbers)
memory.limit(250000) 
#####

#full data
train <- left_join(train_trans,train_id)
test <- left_join(test_trans,test_id)

#for heavy loading memory using
rm(train_id, train_trans,test_id,test_trans); invisible(gc())

glimpse(train)
#lots of NaN and some columns we dont understand

skim_to_list(train) 
skim_to_list(test)
#The features with over 70% missing value in Train are concentrated in D, V, and id columns

#remove the column with 0.99% missing value rate
train$id_07 <- NULL
train$id_08 <- NULL
train$id_21 <- NULL
train$id_22 <- NULL
train$id_23 <- NULL
train$id_24 <- NULL
train$id_25 <- NULL
train$id_26 <- NULL
test['id-07'] <- NULL
test['id-08'] <- NULL
test['id-21'] <- NULL
test['id-22'] <- NULL
test['id-23']<- NULL
test['id-24'] <- NULL
test['id-25'] <- NULL
test['id-26'] <- NULL


train_revised <- knnImputation(full_data)

#detect missing value
missing_trans <- colSums(is.na(train))[colSums(is.na(train)) > 0] %>%
  sort(decreasing = TRUE)
missing_test <- colSums(is.na(test))[colSums(is.na(test)) > 0] %>%
  sort(decreasing = TRUE)


print(paste('The training data has ' ,length(missing_trans) ,'columns out of' , ncol(train), 'having missing values' ))
print(paste('The testing data has ' ,length(missing_test) ,'columns out of' , ncol(test), 'having missing values' ))


#EDA

#View the target variable 
ggplot(train, aes(factor(isFraud), fill = factor(isFraud))) + geom_bar(alpha = 0.8)  + theme_minimal() +
  ggtitle("Target variable") + labs(x = "isFraud") + geom_text(data= train,aes(x = isFraud, label = , y= count(isFraud)),size = 8)
#Extremely imbalanced, 

#View TransactionDT
ggplot(train, aes(TransactionDT, fill = factor(isFraud))) + geom_histogram(alpha = 0.7, bins = 30)  + theme_minimal() +
  ggtitle("Train TransactionDT variable") + labs(x = "TransactionDT") + theme(legend.position = "bottom")

ggplot(test, aes(TransactionDT)) + geom_histogram(alpha = 0.7, bins = 30)  + theme_minimal() +
  ggtitle("Test TransactionDT variable") + labs(x = "TransactionDT")




