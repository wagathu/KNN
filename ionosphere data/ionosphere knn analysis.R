# directory
setwd("C:\\Users\\Admin\\Desktop\\MISCELLANEOUS R\\ml projects\\Knn\\ionosphere data")

# loading data
ionosphere <- read.csv("C:/Users/Admin/Desktop/MISCELLANEOUS R/ml projects/Knn/ionosphere data/ionosphere.txt", header=FALSE, na.strings="", stringsAsFactors=FALSE)

View(ionosphere)

# checking summary
summary(ionosphere)
str(ionosphere)
dim(ionosphere)

# removing unwanted column 2
ionosphere<-ionosphere[c(-1,-2)]

# preparing data for knn
sum(is.na(ionosphere)) # cheking for NA
unique(ionosphere$V35)
ionosphere$V35<-factor(ionosphere$V35, 
                       levels=c("g", "b"), 
                       labels=c("g", "b"))

# test and train datasets
ion_train<-ionosphere[1:251, -32]
ion_test<-ionosphere[252:351, -32]

# test and train labels
ion_train_lab<-ionosphere[1:251, 32]
ion_test_lab<-ionosphere[252:351, 32]

# loding required libraries
library(class)

analyze_k(ion_train, ion_test, ion_train_lab, ion_test_lab, 100)
pred<-knn(ion_train, ion_test, ion_train_lab, k=3)
