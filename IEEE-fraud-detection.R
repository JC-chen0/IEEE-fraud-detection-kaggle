##### Packages
install.packages('tidyverse')
library('tidyverse')
##############################

##### Loading Data
train_trans <- read.csv('data/train_transaction.csv')
train_id <- read.csv('data/train_identity.csv')
test_trans <- read.csv('data/test_transaction.csv')
test_id <- read.csv('data/test_identity.csv')

print('Loading Dataset')
