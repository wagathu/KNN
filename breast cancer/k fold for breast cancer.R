

# k fold train validation -------------------------------------------------

kdd <- nn_kfold(wbcd_train, wbcd_train_labels, fold=117, times=50, k_range = 30)

for (i in 3:ncol(kdd))
{
  kdd[,i] <- ifelse(kdd[,i]==1, "Benign", "Malignant")
}




## comparing using tables
tbl_validation <- list()

for (i in 3:ncol(kdd))
{
  tbl_validation[[i]] <- table(kdd[,i], kdd[,2])
}

val_tibble <- data.frame()
val_tibble[1:30,1] <- 1:30

## calculating accuracy
for (i in 1:length(tbl_validation))
{
  val_tibble[i,2] <- (sum(diag(tbl_validation[[i]])))/length(kdd[,2])*100
}

val_tibble <- val_tibble %>%
  na.omit()

colnames(val_tibble) <- c("k_value", "accuracy")

library(ggplot2)
library(gganimate)
library(plotly)

p <- ggplot(data=val_tibble, aes(x=k_value, y=accuracy))+
  geom_line(col="maroon", lwd=1)+
  labs(title = "ACCURACY OF K VALUES:", x="k values", y="percentage accuracy")

p + transition_reveal(k_value)

ggplotly(p)
# best k as 12, 13 and 14


# instead of using 1 best k we can take a vote on all k's in a certain range

vv <- vector()
for (i in 1:nrow(kdd))
{
  tvec <- kdd[i, 3:ncol(kdd)] %>% 
    as.vector()
  
  vv[i] <- vote(tvec)
  
}
vv <- unlist(vv)

table(vv,kdd$actual)


# choosing only the best performing row combinations of k's ----------------------


posk <- c(12, 9, 13, 8, 7, 11, 10) # the best k's
pp <- kdd[, (posk+2)]
vv <- vector()

for (i in 1:nrow(pp))
{
  tvec <- pp[i,] %>% 
    as.vector()
  
  vv[i] <- vote(tvec)
  
}
vv <- unlist(vv)

table(vv,kdd$actual)



# performing the k fold techniques on the test dataset --------------------

t_pred <- data.frame()

for (i in 1:length(posk))
{
  t_pred[1:nrow(wbcd_test),i] <- knn(train=wbcd_train, test=wbcd_test, 
                    cl=wbcd_train_labels, k=posk[i], prob=T)
}

vv <- vector()
for (i in 1:nrow(t_pred))
{
  temp <- t_pred[i,] %>%
    as.vector()
  vv[i] <- vote(temp)
  
}

vv <- unlist(vv)

table(vv, wbcd_test_labels)


# using k folds of train to predict test ----------------------------------

tp <- test_kfold(wbcd_train, wbcd_test, 
                 wbcd_train_labels, 
                 fold = 117, times=5,
                 k_range = 1:25)
View(tp[,,1])

for (i in 1:dim(tp)[3])
{
  for (j in 1:dim(tp)[2])
  {
    tp[,j,i] <- ifelse(tp[,j,i]==1, "Benign", "Malignant")
    
  }
}

# just testing accuracy values of all k's in all batches

acc <- data.frame()

for (i in 1:dim(tp)[3])
{
  # df is temporary data frame
  df <- tp[,,i]
  
  temp_vec <- vector()
  for (j in 1:ncol(df))
  {
    pred <- df[,j]
    temp_vec[j] <- sum(diag(table(pred, wbcd_test_labels)))/length(wbcd_test_labels)
  }
  print(paste("BATCH NUMBER: ", i))
  print(temp_vec)

  acc[1:ncol(df), i] <- temp_vec; rm(temp_vec)
}
colnames(acc) <- str_c("Batch:", 1:dim(tp)[3])

k_values <- 1:25

acc <- data.frame(cbind(k_values, acc))
View(acc)

p <- ggplot(data=acc)+
  geom_line(aes(x=k_values, y=acc[,2]), col="red")+
  geom_line(aes(x=k_values, y=acc[,3]), col="blue")+
  geom_line(aes(x=k_values, y=acc[,4]), col="green")+
  geom_line(aes(x=k_values, y=acc[,5]), col="maroon")+
  geom_line(aes(x=k_values, y=acc[,6]), col="yellow")

ggplotly(p)
p+transition_reveal(k_values)


# using the best k values discovered in k fold training(posk) to test dataset
library(magrittr) # working with pipes

posk_acc <- vector()
for (i in 1:dim(tp)[3])
{
  temp_df <- tp[,posk,i]
  posk_vote <- vector()
  
  for (j in 1:nrow(temp_df))
  {
    posk_vote[j] <- vote(temp_df[j,]) %>%
      unlist()
  }
  message(paste("Testing Batch: ",i))
  print(paste("Posk vote has length: ", length(posk_vote)))
  posk_acc[i] <- sum(diag(table(posk_vote, wbcd_test_labels)))/length(wbcd_test_labels)
  
}

# highest accuracy recorded of the batches is : 98 %

