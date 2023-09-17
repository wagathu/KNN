## knn assignment

setwd("C://Users//Admin//Desktop//MISCELLANEOUS R//ml projects//knn")

## loading required pakages
library(pacman)
p_load(dplyr, ggplot2, gmodels, class)

## loading dataset
dd<-iris
View(dd)

colnames(dd)<-c("sl", "sw", "pl", "pw", "sp")

## randomizing their appearance
row<-sample(150)
d<-data.frame()
for (i in 1:150)
{
  d[i, 1:5]<-dd[row[i], 1:5]
}

## plotting the species versus sl, sw, pl, pw
ggplot(data=d, aes(x=sp, y=sl))+
  labs(x="Species", y="sepal length")+
  geom_boxplot()

ggplot(data=d, aes(x=sp, y=sw))+
  labs(x="Species", y="sepal width")+
  geom_boxplot()

ggplot(data=d, aes(x=sp, y=pl))+
  labs(x="Species", y="Petal length")+
  geom_boxplot()

ggplot(data=d, aes(x=sp, y=pw))+
  labs(x="Species", y="Petal Width")+
  geom_boxplot()


##preparing data for knn

## standardizing
norm<-function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}

d_s<-d
for (i in 1:4)
{
d_s[, i]<-norm(d[, i])
}

class(d$sp) # ensuring its a factor

d_train<-d_s[1:100, -5]
d_test<-d_s[101:150, -5]

## ensuring no missig values
sum(is.na(d)) # if 0 then no NA 

d_train_labels<-d_s[1:100, 5]
d_test_labels<-d_s[101:150, 5]

View(d_test);View(d_train)

d_test_pred<-knn(d_train, d_test, d_train_labels, k=15)

a<-CrossTable(d_test_pred, d_test_labels, prop.chisq = F)
cm = table(d_test_pred, d_test_labels)

analyze_k(d_train, d_test, d_train_labels, d_test_labels)
