#kNN Machine learning algorithm 1
# 100daysofMLcode

# loading packages
library(pacman)
p_load(class, tibble, gmodels, dplyr, gganimate, ggplot2, plotly)

# working directory
setwd("C:/Users/stanley/Desktop/MISCELLANEOUS R/ml projects/Knn/breast cancer")

# loading dataset
wbcd<-read.csv("breast cancer dataset.csv", as.is=T)
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

# buildingbtwo datasets, the training and testing one
div_df <- sample(nrow(wbcd_n), 100, replace = F)

wbcd_train<-wbcd_n[-div_df, -1]
wbcd_test<-wbcd_n[div_df, -1]

# creating factor vectors to store desired 
#features(diagnosis) from both test and train
wbcd_train_labels<-wbcd[-div_df, 1]
wbcd_test_labels<-wbcd[div_df, 1]

# maintaining the original form of the data
wbcd_train <- wbcd_n[1:469, -1]
wbcd_test <- wbcd_n[470:569, -1]

wbcd_train_labels <- wbcd_n[1:469, 1]
wbcd_test_labels <- wbcd_n[470:569, 1]

# trying hassanat distance
acc <- vector()
for (i in 1:25)
{
  message("Running k: ", i)
  haspred <- nn_of(train = wbcd_train, test = wbcd_test, train_labs = wbcd_train_labels,
                   distance = "hassanat", k=i)
  hp <- ifelse(haspred==1, "Benign", "Malignant")
  acc[i] <- mean(wbcd_test_labels == hp)
}

plot(acc, type="b", col="maroon", lwd=2)

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
# if 0, no NA values exist


## leave one out validation
ddl <- knn_validation(wbcd_train, wbcd_train_labels, 1:30)
ddk <- nn_kfold(wbcd_train, wbcd_train_labels, fold=117, times=10, k_range=30)


wbcd_test_pred<-knn(train=wbcd_train, test=wbcd_test, 
                    cl=wbcd_train_labels, k=21, prob=T)
as <- table(wbcd_test_pred, wbcd_test_labels)


knn_pred <- nn_of(wbcd_train,wbcd_test,wbcd_train_labels,k=5)
vnn_pred<-vknn_of(wbcd_train,wbcd_test,wbcd_train_labels,k=5)

# assessing accuracy of add train using matrix
table(vnn_pred, wbcd_test_labels)


# testing if add train is valid -------------------------------------------

tr <- wbcd_train
te <- wbcd_test
trl <- wbcd_train_labels
tel <- wbcd_test_labels
pp <- vector()

for (i in 1:nrow(te))
{
  pp[i] <- knn(train=tr, test=te[i,], 
               cl=trl, k=5, prob = T)
  print(paste("label: ",pp[i]))
  
  if (pp[i] == 1)
  {
    tr <- rbind(tr, te[i,])
    trl <- c(trl, tel[i])
  }
  else
  {
    
  }
  # 2 - malignant
  #tr <- ifelse(pp[i]==2, rbind(tr, te[i,]), tr)
  #trl <- ifelse(pp[i]==2, c(trl, tel[i]), trl)
  #tr <- rbind(tr, te[i,])
  #trl <- c(trl, tel[i])
  message(paste("Train instances now at: ", nrow(tr)))
}
table(pp, wbcd_test_labels)

# loopin thru k to find best k for add train
acc <- vector()
for (i in 1:25)
{
  message(paste("TRAINING AT K: ", i))
  temp <- vknn_of(wbcd_train,wbcd_test,wbcd_train_labels,k=i)
  acc[i] <- sum(diag(table(temp, wbcd_test_labels)))
  rm(temp)
}
plot(acc, type="o", col="maroon", lwd=2, 
     main="Accuracy of K values", xlab="K values", ylab="% Accuracy")
obs <- sum(diag(as))
axis(1, at=1:25)
abline(h=obs, col="blue")
# acc is at k=5

# evaluating model performance
agreement <- function(actual, predicted)
{
  for (i in 1:length(actual))
  {
    if (actual[i] != predicted[i])
    {
      message(paste("Disagreement at :", i))
    }
  }
}

vnn_pred <- ifelse(vnn_pred == 1, "Benign", "Malignant")
agreement(actual=wbcd_test_labels, predicted = vnn_pred)

# looking at how much the predicted and actual factor of 
#features match up
a<-CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
              prop.chisq=FALSE)


## we are trying the z standardisation over normalisation
wbcd_z<-as.data.frame(scale(wbcd[-1]))
View(wbcd_z)

# confirming that it has standardised
summary(wbcd_z[c("radius_mean", "area_mean", "smoothness_mean")])
## the mean of a z score shld always be 0

## performing knn
wbcd_train<-wbcd_z[1:469, ]
wbcd_test<-wbcd_z[470:569, ]
wbcd_train_labels<-wbcd[1:469, 1]
wbcd_test_labels<-wbcd[470:569, 1]

wbcd_test_pred<-knn(wbcd_train, wbcd_test,
                    cl=wbcd_train_labels, k=4)

nntest <- nn_of(train = wbcd_train, test = wbcd_test, train_labs = wbcd_train_labels,
                  distance = "chebyshev", k=4)
table(nntest, wbcd_test_labels)

a<-CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq = F)


a<-analyze_k(wbcd_train, wbcd_test, wbcd_train_labels, wbcd_test_labels, 50)


# database a holds test accuracy
# database dd holds train validation

cmptbl <- as.data.frame(cbind(a$`k values`, a$Percentage_accuracy, dd$accuracy))
colnames(cmptbl) <- c("k", "test_acc", "train_acc")

pp <- ggplot(data=cmptbl)+
  geom_line(aes(x=k, y=test_acc), col="maroon", lwd=1)+
  geom_line(aes(x=k, y=train_acc), col="blue", lwd=1)+
  labs(title="TRAIN vs TEST ACCURACY", x="k values", y="accuracy")
  
ggplotly(pp)
pp+transition_reveal(k)

## residuals
cmptbl$res <- (cmptbl$test_acc - cmptbl$train_acc)

# plotting the residuals
pp_res <- ggplot(data=cmptbl)+
  geom_point(aes(x=k, y=res), col="maroon")+
  labs(title="TRAIN || TEST RESIDUALS", x="k values", y="residuals")+
  geom_hline(aes(yintercept=0))+
  geom_text(aes(x=k, y=res, label=k))+
  theme_classic()
ggplotly(pp_res)

# foward selection for feature engineering
qq <- knn_fwd(train_data=wbcd_train, 
              test_data=wbcd_test, 
              train_labs = wbcd_train_labels, 
              test_labs = wbcd_test_labels, 
              k_neighbours = 21)

a<-analyze_k(wbcd_train[, -c(24)], wbcd_test[, -c(24)], wbcd_train_labels, wbcd_test_labels, 50)

obm<-knn(train=wbcd_train[,-27], test=wbcd_test[,-27], 
                    cl=wbcd_train_labels, k=21, prob=T)
table(obm, wbcd_test_labels)


# Diagnosing k errors in rows instances of train --------------------------

diagnose_k(train=wbcd_train, test=wbcd_test,
train_labs = wbcd_train_labels, test_labs = wbcd_test_labels,
k_value = 5)

wbcd_train_labels[c(428,284,67,438,274)]
wbcd_train_labels[c(208,365,447,50,115)]
wbcd_test_labels[13]
wbcd_test_labels[54]

# omitting train points to see if model is improved
om <- c(428,284,67,438,274,208,365,447,50,115)
wtrl <- wbcd_train_labels[-om]
wtrd <- wbcd_train[-om,]

ompred <- knn(wtrd, wbcd_test,cl=wtrl,k=5)
table(ompred,wbcd_test_labels)

diagnose_k(wtrd,wbcd_test,wtrl,wbcd_test_labels,5)

# conclusion, these two points at 13 and 54 are completely defined


# principal components analysis -------------------------------------------

ptrain <- prcomp(wbcd_train)$x
ptest <- prcomp(wbcd_test)$x

plabs <- knn(train=ptrain, test=ptest, 
             cl=wbcd_train_labels, k=21, prob=T)
table(plabs, wbcd_test_labels)
