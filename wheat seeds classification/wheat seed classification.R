## knn classification of wheat seeds

# libraries
library(pacman)
p_load(class, gmodels, plotly, magrittr, ggplot2, stringr)


# directory
setwd("C:/Users/stanley/Desktop/MISCELLANEOUS R/ml projects/knn/wheat seeds classification")

# data
seeds_dataset <- read.delim("C:/Users/stanley/Desktop/MISCELLANEOUS R/ml projects/Knn/wheat seeds classification/seeds_dataset.txt",
                            header=FALSE, na.strings="0", stringsAsFactors=FALSE)
View(seeds_dataset)
colnames(seeds_dataset)<-c("area", "perimeter", "compactness", "kernel_length", "kernel_width", 
                       "ass_coef", "kernelG_length", "class")

## cheking summary
summary(seeds_dataset)
dim(seeds_dataset)

## omitting na values
ws<-na.omit(seeds_dataset)

summary(ws)
dim(ws)

sum(is.na(ws)) ## cheking NA's

set.seed(371)
rand<-sample(1:nrow(ws))

ws_new<-data.frame()
for (i in 1:nrow(ws))
{
  ws_new[i, 1:ncol(ws)]<-ws[rand[i], 1:ncol(ws)]
}

View(ws_new)

## creating train and test datasets
ws_new_train<-ws_new[1:100, -8]
ws_new_test<-ws_new[101:199, -8]

unique(ws_new$class)
ws_new$class<-factor(ws_new$class, levels=c(1,2,3), labels=c(1,2,3))

ws_new_trainlab<-ws_new[1:100, 8]
ws_new_testlab<-ws_new[101:199, 8]

norm_z<-function(x)
{ ## normalization
  return((x-mean(x))/sd(x))
}

ws_new_train<-as.data.frame(lapply(ws_new_train, norm_z))
ws_new_test<-as.data.frame(lapply(ws_new_test, norm_z))

ws_new_testpred<-knn(ws_new_train, ws_new_test, ws_new_trainlab, k=20)
table(ws_new_testpred, ws_new_testlab)

a <- analyze_k(ws_new_train, ws_new_test, 
           ws_new_trainlab, ws_new_testlab, 30)


# using add_train variant of knn
acc <- vector()
# values of k to use
for (i in 1:25)
{
  message(paste("TRAINING AT K: ", i))
  temp <- vknn_of(ws_new_train, ws_new_test, ws_new_trainlab, k=i)
  acc[i] <- sum(diag(table(temp, ws_new_testlab)))
  rm(temp)
}
plot(acc, type="l", col="blue", lwd=2, 
     main="Accuracy of K values", xlab="K values", ylab="% Accuracy")
# best k is 20 and 22 at 93%

vknnpred<-vknn_of(ws_new_train, ws_new_test, ws_new_trainlab, k=20)

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

agreement(actual = ws_new_testlab, predicted = vknnpred)
