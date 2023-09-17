
# loocv -------------------------------------------------------------------

loocv_db <- knn_validation(train_dataset = ws_new_train, 
                           train_lab = ws_new_trainlab, 
                           k_range = 1:30)

# highest accuracy 95 %

p <- ggplot(data=loocv_db, aes(x=k_value, y=accuracy))+
  geom_line(col="maroon", lwd=1.2, )+
  labs(title = "ACCURACY OF K VALUES:", x="k values", y="percentage accuracy")

p + theme_classic()  + transition_reveal(k_value)
ggplotly(p)


# k fold validation -------------------------------------------------------

kfold_db <- nn_kfold(ws_new_train, ws_new_trainlab, fold=20, times=50, k_range = 30)

## comparing using tables
tbl_validation <- list()

for (i in 3:ncol(kfold_db))
{
  tbl_validation[[i]] <- table(kfold_db[,i], kfold_db[,2])
}

val_tibble <- data.frame()
val_tibble[1:30,1] <- 1:30

## calculating accuracy
for (i in 1:length(tbl_validation))
{
  val_tibble[i,2] <- (sum(diag(tbl_validation[[i]])))/length(kfold_db[,2])*100
}

val_tibble <- val_tibble %>%
  na.omit()

colnames(val_tibble) <- c("k_value", "accuracy")

library(ggplot2)
library(gganimate)
library(plotly)

p <- ggplot(data=val_tibble)+
  geom_line(aes(x=k_value, y=accuracy), col="maroon", lwd=1)+
  labs(title = "ACCURACY OF K VALUES:", x="k values", y="percentage accuracy")

p + transition_reveal(k_value)

ggplotly(p) # 92.5 % accuracy obtained

# instead of using 1 best k we can take a vote on all k's in the k_range

vv <- vector()
for (i in 1:nrow(kfold_db))
{
  tvec <- kfold_db[i, 3:ncol(kfold_db)] %>% 
    as.vector()
  
  vv[i] <- vote(tvec)
  
}
vv <- unlist(vv)

table(vv,kfold_db$actual)
# 91.4 % accuracy

# choosing only the best performing row combinations of k's ----------------------

posk <- c(9,5,11,12,10,13,3) # the best k's from validation_tibble
pp <- kfold_db[, (posk+2)]
vv <- vector()

for (i in 1:nrow(pp))
{
  tvec <- pp[i,] %>% 
    as.vector()
  
  vv[i] <- vote(tvec)
  
}
vv <- unlist(vv)

table(vv,kfold_db$actual)
# 92.3 % accuracy


# k fold train batches to test --------------------------------------------

tp <- test_kfold(ws_new_train, ws_new_test, 
                 ws_new_trainlab, 
                 fold = 20, times=50,
                 k_range = 1:35)

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
    temp_vec[j] <- sum(diag(table(pred, ws_new_testlab)))/length(ws_new_testlab)
  }
  message(paste("BATCH NUMBER: ", i))
  print(temp_vec)
  
  acc[1:ncol(df), i] <- temp_vec; rm(temp_vec)
}
colnames(acc) <- str_c("Batch:", 1:dim(tp)[3])

k_values <- 1:35

acc <- data.frame(cbind(k_values, acc))
View(acc)

# using the best k values discovered in k fold training(posk) to test dataset

posk <- c(9,5,11,12,10,13,3) # the best k's from training validation

library(magrittr) # working with pipes

posk_acc <- vector()
for (i in 1:dim(tp)[3]) # looping thru' batches
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
  posk_acc[i] <- sum(diag(table(posk_vote, ws_new_testlab)))/length(ws_new_testlab)
  
}

# highest accuracy is: 91 %

