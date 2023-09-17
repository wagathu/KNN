# directory
setwd("C:\\Users\\Admin\\Desktop\\MISCELLANEOUS R\\ml projects\\Knn\\Housing data")

# loading data
house_data <- read.table("C:/Users/Admin/Desktop/MISCELLANEOUS R/ml projects/Knn/Housing data/housing.txt",
                         quote="\"", comment.char="", na.strings="", stringsAsFactors=FALSE)
View(house_data)

#summary
summary(house_data)
str(house_data)

# preparing data for linear regression
house_model<-lm(V14~., data=house_data)
summary(house_model)




