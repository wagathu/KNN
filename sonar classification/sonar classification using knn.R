## knn classification of sonar data

# directory
setwd("C:/Users/Admin/Desktop/MISCELlANEOUS R/ml projects/Knn/sonar classification")

# dataset
sonar_data<-read.csv("sonar data.csv", header=F, as.is=T)
View(sonar_data)

# loading packages
library(pacman)
p_load(tibble, class, gmodels)

dim(sonar_data)
glimpse(sonar_data)

# cheking NA's
sum(is.na(sonar_data))

# changing output variable to factor
class(sonar_data[, 61])
unique(sonar_data[, 61])

sonar_data$V61<-factor(sonar_data$V61, 
                       levels=c("R", "M"), labels=c("R", "M"))

# preparing data for knn
# randomizing
sdata<-data.frame()
rand<-sample(1:nrow(sonar_data))
for (i in 1:nrow(sonar_data))
{
  sdata[i, 1:ncol(sonar_data)]<-sonar_data[rand[i], 1:ncol(sonar_data)]
}

# normalizing datacolumns function
norm_z<-function(x)
{
  return((x-mean(x))/sd(x))
}

View(sdata)

# creating test and train datasets
sdata_train<-sdata[1:168, -61]
sdata_test<-sdata[169:208, -61]

sdata_train<-as.data.frame(lapply(sdata_train, norm_z))
sdata_test<-as.data.frame(lapply(sdata_test, norm_z))

sdata_train_lab<-sdata[1:168, 61]
sdata_test_lab<-sdata[169:208, 61]

sdata_test_pred<-knn(sdata_train, sdata_test, sdata_train_lab, k=2)

CrossTable(sdata_test_pred, sdata_test_lab)

source("C:/Users/Admin/Desktop/MISCELLANEOUS R/ml projects/Knn/analyze_k.R")
d<-analyze_k(sdata_train, sdata_test, sdata_train_lab, sdata_test_lab, k_range=100)

library(ggplot2)
library(plotly)
library(gridExtra)

pa<-ggplotly(ggplot(data=d, aes(x=`k values`, y=Percentage_accuracy))+
geom_line(color="blue")+theme_gray())

pe<-ggplotly(ggplot(data=d, aes(x=`k values`, y=Percentage_error))+
geom_line(color="red")+theme_gray()+geom_smooth(se=F, method="loess"))

grid.arrange(pa,pe,ncol=2)