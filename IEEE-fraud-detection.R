##### Packages
install.packages('tidyverse')
library('tidyverse')
library('ggplot2')
library(dplyr)
library('skimr')

# ##### Using python
# install.packages('reticulate')
# library('reticulate')
# np <- import('numpy')
# pd <- import("pandas")

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
test <- read_csv('test.csv')
#load data
train <- left_join(train_trans,train_id)
test <- left_join(test_trans,test_id)
rm(train_id, train_trans,test_id,test_trans); invisible(gc())

colnames(test)[which(names(test) =="id-01")] <-"id_01"
colnames(test)[which(names(test) =="id-02")] <-"id_02"
colnames(test)[which(names(test) =="id-03")] <-"id_03"
colnames(test)[which(names(test) =="id-04")] <-"id_04"
colnames(test)[which(names(test) =="id-05")] <-"id_05"
colnames(test)[which(names(test) =="id-06")] <-"id_06"
colnames(test)[which(names(test) =="id-09")] <-"id_09"
colnames(test)[which(names(test) =="id-10")] <-"id_10"
colnames(test)[which(names(test) =="id-11")] <-"id_11"
colnames(test)[which(names(test) =="id-12")] <-"id_12"
colnames(test)[which(names(test) =="id-13")] <-"id_13"
colnames(test)[which(names(test) =="id-14")] <-"id_14"
colnames(test)[which(names(test) =="id-15")] <-"id_15"
colnames(test)[which(names(test) =="id-16")] <-"id_16"
colnames(test)[which(names(test) =="id-17")] <-"id_17"
colnames(test)[which(names(test) =="id-18")] <-"id_18"
colnames(test)[which(names(test) =="id-19")] <-"id_19"
colnames(test)[which(names(test) =="id-20")] <-"id_20"
colnames(test)[which(names(test) =="id-28")] <-"id_28"
colnames(test)[which(names(test) =="id-29")] <-"id_29"
colnames(test)[which(names(test) =="id-30")] <-"id_30"
colnames(test)[which(names(test) =="id-31")] <-"id_31"
colnames(test)[which(names(test) =="id-32")] <-"id_32"
colnames(test)[which(names(test) =="id-33")] <-"id_33"
colnames(test)[which(names(test) =="id-34")] <-"id_34"
colnames(test)[which(names(test) =="id-35")] <-"id_35"
colnames(test)[which(names(test) =="id-36")] <-"id_36"
colnames(test)[which(names(test) =="id-37")] <-"id_37"
colnames(test)[which(names(test) =="id-38")] <-"id_38"

write_csv(test,"test1.csv")

#lots of NaN and some columns we dont understand
skim_to_list(train) 
skim_to_list(test)
#The features with over 70% missing value in Train are concentrated in D, V, and id columns

train_missing_percent=(colSums(is.na(train))/nrow(train))*100
train_miss_Percent_DF=data.frame(colnames(train),train_missing_percent)
colnames(train_miss_Percent_DF)<-c("Variable","MissPercentage")
train_miss_Percent_DF=train_miss_Percent_DF[order(train_miss_Percent_DF$MissPercentage,decreasing = TRUE), ]
plot(train_miss_Percent_DF$MissPercentage, ylab = "% Missing", main = "Percentage of Missing data")

over_99_miss_percentage_col <- c("id_24","id_25","id_07","id_08","id_21","id_26","id_22","id_23","id_27") 
for(i in over_99_miss_percentage_col){
  train[,i] = NULL
}

test_missing_percent=(colSums(is.na(test))/nrow(test))*100
test_miss_Percent_DF=data.frame(colnames(test),test_missing_percent)
colnames(test_miss_Percent_DF)<-c("Variable","MissPercentage")
test_miss_Percent_DF=test_miss_Percent_DF[order(test_miss_Percent_DF$MissPercentage,decreasing = TRUE), ]
plot(test_miss_Percent_DF$MissPercentage, ylab = "% Missing", main = "Percentage of Missing data")
train_over_99_miss_percentage_col <- c("id-24","id-25","id-07","id-08","id-21","id-26","id-22","id-23","id-27") 
for(i in train_over_99_miss_percentage_col){
  test[,i] = NULL
}

write.csv(train,'train.csv')
write.csv(test,'test.csv')


#The code below is for category , but I turn to using python.
#I've try knn imputaion or using naIndexes to deal with missing value, but it is easier to do in python.

# category_cols <- c('ProductCD', 'card1', 'card2', 'card3', 'card4', 'card5', 'card6',
#                    'addr1', 'addr2', 'P_emaildomain', 'R_emaildomain', 'DeviceType', 'DeviceInfo',
#                    'M1','M2','M3','M4','M5','M6','M7','M8','M9',"id_12",
#                    "id_13","id_14","id_15","id_16","id_17","id_19","id_20",
#                    "id_28","id_29","id_30","id_31","id_32","id_33",
#                    "id_34","id_35","id_36","id_37","id_38")
# column_names <- colnames(train)
# num_cols = setdiff(column_names,category_cols)
# train_without_category_cols <- train[num_cols]
# 
# missing_trans_without_category_cols <- colSums(is.na(train_without_category_cols))[colSums(is.na(train_without_category_cols)) > 0] %>%
#   sort(decreasing = TRUE)
# 
# # source_python('medianImputation.py')
# # train <- naImputation(train)
# #missingImputation <- knnImputation(data = train[num_cols],k=2)
# #Not sufficient complete cases for computing neighbors.
# 
# 
# #detect missing value
# 
# #Filling NA value for category variables
# train_category_cols <- train[category_cols] 
# 
# train_category_cols[is.na(train_category_cols)] <- "missing"
# 
# missing_test <- colSums(is.na(test))[colSums(is.na(test)) > 0] %>%
#   sort(decreasing = TRUE)
# 
# 
# print(paste('The training data has ' ,length(missing_trans) ,'columns out of' , ncol(train), 'having missing values' ))
# print(paste('The testing data has ' ,length(missing_test) ,'columns out of' , ncol(test), 'having missing values' ))




