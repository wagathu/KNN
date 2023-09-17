## knn to classify wine quality

## working directory
setwd("C:/Users/stanley/Desktop/MISCELLANEOUS R/ml projects/Knn/wine classification")

## loading file
wq_dataset <- read.csv("wine quality.txt",
               sep=";", as.is=TRUE)
View(wq_dataset) # wq - wine quality

# seed forreproducibility
set.seed(34)
random<-sample(1:nrow(wq_dataset))
wq<-data.frame()
for (i in 1:nrow(wq_dataset))
{
  wq[i, 1:ncol(wq_dataset)]<-wq_dataset[random[i], 1:ncol(wq_dataset)]
}
write.csv(wq, file="wine quality.csv")
summary(wq) ## summary
dim(wq) ## dimensions
str(wq) ## cjecking datatypes

# checking the distribution of output variable
table(wq$quality)

# omitting 3 and 9
wq$quality <- ifelse(wq$quality == 9 , NA, wq$quality)
wq$quality <- ifelse(wq$quality == 3 , NA, wq$quality)

wq <- na.omit(wq)

# checking for NA
sum(is.na(wq))

# setting quality to be a factor
unique(wq$quality)
# wq$quality<-factor(wq$quality, levels=c(6,5,7,8,4,3,9), 
#                   labels=c(6,5,7,8,4,3,9))

## cheking missing values
sum(is.na(wq))

## loading packages
library(pacman)
p_load(class, gmodels)

dim(wq)
# creating train and test datasets
wq_train<-wq[1:4673, -12]
wq_test<-wq[4674:4873, -12]

# creating test and train labels
wq_train_lab<-wq[1:4673, 12]
wq_test_lab<-wq[4674:4873, 12]

## standardising the numerical vectors in train and test datasets
std<-function(x)
{ ## standardisation
  return((x-min(x))/(max(x)-min(x)))
}
norm<-function(x)
{ ## normalization
  return((x-mean(x))/sd(x))
}

wq_train <- as.data.frame(lapply(wq_train[1:11], std))
wq_test <- as.data.frame(lapply(wq_test[1:11], std))


# train validation --------------------------------------------------------
# leave one out
dhold <- knn_validation(wq_train, wq_train_lab, 1:75)

p_load(ggplot2, plotly, gganimate)


p <- ggplot(data=dhold, aes(x=k_value, y=accuracy))+
  geom_line(col="maroon", lwd=1)+
  labs(title = "ACCURACY OF K VALUES:", x="k values", y="percentage accuracy")

p + theme_classic()  + transition_reveal(k_value)
ggplotly(p)


wq_test_pred<-knn(wq_train, wq_test, wq_train_lab, k=32)

# using add_train variant of knn
acc <- vector()
# values of k to use
for (i in 1:25)
{
  message(paste("TRAINING AT K: ", i))
  temp <- vknn_of(wq_train, wq_test, wq_train_lab, k=i)
  acc[i] <- sum(diag(table(temp, wq_test_lab)))
  rm(temp)
}
# result is acc=c(78L, 75L, 79L, 89L, 96L, 94L, 104L, 109L, 103L, 96L, 102L, 
#100L, 96L, 97L, 95L, 93L, 94L, 93L, 90L, 94L, 39L, 38L, 39L, 
#27L, 36L, 35L, 39L, 28L, 39L, 29L, 28L, 27L, 25L, 25L, 25L, 28L, 
#27L, 36L, 39L, 37L, 102L, 29L, 25L, 28L, 33L, 101L, 105L, 108L, 
#28L, 30L)
# highest accuracy was 54.5

# checking accuracy
zz<-CrossTable(wq_test_pred, wq_test_lab, 
           prop.chisq = F)
table(wq_test_pred, wq_test_lab)

# analyzing range of k values
aa <- analyze_k(wq_train,wq_test,wq_train_lab,wq_test_lab, 100)



# selection, foward -------------------------------------------------------

ff <- knn_fwd(train_data = wq_train,
              test_data = wq_test, train_labs = wq_train_lab,
              test_labs = wq_test_lab, k_neighbours = 32)

ff2 <- knn_fwd(train_data = wq_train[,-c(3,8,10)],
              test_data = wq_test[, -c(3,8,10)], train_labs = wq_train_lab,
              test_labs = wq_test_lab, k_neighbours = 32)

aa <- analyze_k(wq_train[,-c(3,8,10)], 
                wq_test[,-c(3,8,10)], 
                wq_train_lab,wq_test_lab, 100)
