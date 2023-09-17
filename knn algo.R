#kNN Machine learning algorithm 1
# 100daysofMLcode

# loading packages
library(pacman)
pacman::p_load(pacman, class, tibble)

# working directory
setwd("C:/Users/Admin/Desktop/MISCELLANEOUS R/ml projects/Knn")

# loading dataset
wbcd<-read.csv(file.choose(), as.is=T)
View(wbcd)

# a glimpse of data
glimpse(wbcd)

# removing ID column
wbcd<-wbcd[, -1]
View(wbcd)

# changing target feature(diagnosis) to factor
wbcd$diagnosis<-factor(wbcd$diagnosis, 
                       levels=c("B", "M"), 
                       labels=c("Benign", "Malignant"))

# confirming that target feature is a factor
round(prop.table(table(wbcd$diagnosis)*100), digits=1)

# taking  closer look at three features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# transforming and normalizing numerical data

#writing a normalization funxtion
norm<-function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}

wbcd_n<-wbcd
for (j in 2:31)
{
  wbcd_n[ ,j]<-norm(wbcd_n[ ,j])
}
View(wbcd_n)

# testing normalization
summary(wbcd_n[c("radius_mean", "area_mean", "smoothness_mean")])

# building two datasets, the training and testing one
wbcd_train<-wbcd_n[1:469, -1]
wbcd_test<-wbcd_n[470:569, -1]

# creating factor vectors to store desired 
#features(diagnosis) from both test and train
wbcd_train_labels<-wbcd[1:469, 1]
wbcd_test_labels<-wbcd[470:569, 1]

#kNN implementation from class packge
library(class)
# we'll use k=21 since training data has
#469 observations, sqare rooting that...
#try to always use k=odd no
library(gmodels)


## before running knn we remove the diagnosis
## or feature we are interested in

# to ensure no NAs in the data we do
sum(is.na(wbcd_test))
sum(is.na(wbcd_train))
# if 0, you good


wbcd_test_pred<-knn(train=wbcd_train[, -1], test=wbcd_test[, -1], 
                    cl=wbcd_train_labels, k=21, prob=T)

# evaluating model performance

# looking at how much the predicted and actual factor of 
#features match up
a<-CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE, prop.r = F, prop.c = F, prop.t = F)


#### trying to build a knn model which can track percentage accuracy
####################################################################
per_acc<-data.frame()
per_acc[1:100, 1]<-1:100
for (i in 1:100)
{ ### storing predicted columns
  per_acc[, i]<-knn(wbcd_train, wbcd_test, wbcd_train_labels, k=i)
}
a<-list()
for (i in 1:100)
{ 
  a[[i]]<-CrossTable(per_acc[, i], wbcd_test_labels, prop.chisq = F)
}

per_acc_digit<-data.frame()
per_acc_digit[1:100, 1]<-1:100
for (i in 1:100)
{
  per_acc_digit[i, 2]<-(s<-as.data.frame(a[[i]][1]))[1, 3]+
    (s<-as.data.frame(a[[i]][1]))[4, 3]
}

plot(per_acc_digit[, 2], type="l", col="red", lwd=2, 
     main="A PLOT TO SHOW PERCENTAGE ACCURACY OF K VALUES", xlab="k values", 
     ylab="Percentage accuracy", ylim=c(90, 100))
grid(50)
abline(h=max(per_acc_digit[, 2]), col="blue")
