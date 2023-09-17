## knn classification of authentic bank notes

# directory
setwd("C:/Users/Admin/Desktop/MISCELLANEOUS R/ml projects/knn/banknote authentication")

# data
bank_data<-read.csv("data_banknote_authentication.txt", sep=",", 
                    as.is=T, header=F)
View(bank_data)
colnames(bank_data)<-c("Variance", "Skewness", "Kurtosis", "Entropy", "Class")

## loading packages
library(class)
library(gmodels)

# summary
summary(bank_data)

random<-sample(1:nrow(bank_data))
bdata<-data.frame()
for (i in 1:length(random))
{
  bdata[i, 1:ncol(bank_data)]<-bank_data[random[i], 1:ncol(bank_data)]
}

View(bdata)

dim(bdata) ## cheking rownumber an colnumber

## cheking NA's
sum(is.na(bdata))

## cheking output variable datatype
class(bdata$Class)
## changing to factor
unique(bdata$Class)
bdata$Class<-factor(bdata$Class, levels=c(0,1), labels=c(0,1))

## dividing into train and test
bdata_train<-bdata[1:1271, -5]
bdata_test<-bdata[1272:1372, -5]

## creating train and test labels
b_data_train_lab<-bdata[1:1271, 5]
b_data_test_lab<-bdata[1272:1372, 5]

norm_z<-function(x)
{ ## normalization
  return((x-mean(x))/sd(x))
}

bdata_train<-as.data.frame(lapply(bdata_train, norm_z))
bdata_test<-as.data.frame(lapply(bdata_test, norm_z))

bdata_test_pred<-knn(bdata_train, bdata_test, b_data_train_lab, k=15)
table(bdata_test_pred, b_data_test_lab)

nn<-nn_of(bdata_train,bdata_test,b_data_train_lab,k=15)
table(as.character(nn), b_data_test_lab)

analyze_k(bdata_train, bdata_test, b_data_train_lab, b_data_test_lab, 100)
