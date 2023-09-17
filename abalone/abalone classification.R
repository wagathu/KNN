## knn classification of abalone data

# directory
setwd("C:/Users/stanley/Desktop/MISCELLANEOUS R/ml projects/Knn/abalone")

# loading packages
library(pacman)
p_load(tibble, class, gmodels)

# dataset
abdata<-read.csv("abalone.csv", as.is=T, header=F)
View(abdata)

colnames(abdata)<-c("sex", "length", "diameter", "height", "w_height", "s_weight", "v_weight", 
                     "sh_weight", "rings")

## rings is the dependent variable, and not age

rand<-sample(1:nrow(abdata))
abalone<-data.frame()

# randomizing the data
for (i in 1:nrow(abdata))
{
  abalone[i, 1:ncol(abdata)]<-abdata[rand[i], 1:ncol(abdata)]
}

# summary
dim(abalone)

summary(abalone)

table(abalone$sex)

# corelation test
# cor(abalone[-1], method="kendall")

## preparing data for knn
# cheking NA's
sum(is.na(abalone))

# changing output variable to factor
class(abalone$sex)
unique(abalone$sex)

abalone$sex<-factor(abalone$sex, 
                    levels=c("M", "F", "I"), labels=c("M", "F", "I"))

## normalizing train and test datasets
sex <- abalone[,1]
rings <- abalone[,9]

norm_z<-function(x)
{
  return((x-mean(x))/sd(x))
}
abalone<-as.data.frame(lapply(abalone[-c(1,9)], norm_z))
abalone <- cbind(abalone, sex, rings)

## creating test and train datasets
ab_train<-abalone[1:4077, -9]
ab_test<-abalone[4078:4177, -9]

ab_train_lab<-abalone[1:4077, 9] %>%
  as.factor()
ab_test_lab<-abalone[4078:4177, 9] %>%
  as.factor()

pred<-knn(ab_train, ab_test, ab_train_lab, k=7)

CrossTable(ab_test_pred, ab_test_lab, prop.chisq = F)

## predicting using k nearest neighbours
d <- analyze_k(ab_train, ab_test, ab_train_lab, ab_test_lab, k_range = 50)
