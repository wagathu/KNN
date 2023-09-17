# k fold cross validation -------------------------------------------------

nn_kfold <- function(train_dataset, train_labels, fold, times, k_range)
{
  # fold - the k to use in k fold
  # times - the number of k batches to model on
  # k - The k to use in k nearest neighbours
  
  require(class)
  n <- nrow(train_dataset)
  
  # matrix to hold the k folds
  set.seed(328)
  s <- sample(n, (fold*times), replace = T)
  amat <- matrix(s, nrow=times, byrow=T)
  
  # to hold the values of the validations
  
  v_tbl <- data.frame()
  
  for (kval in 1:k_range)
  {
    message(paste("Using k value: ", kval))
    pred_val <- vector()
    for (i in 1:nrow(amat))
    {
      rand <- amat[i,]
      temp_pred <- knn(train=train_dataset[-rand,], 
                       test=train_dataset[rand,], 
                       cl=train_labels[-rand], 
                       k=kval)
      
      pred_val <- c(pred_val, temp_pred)
      rm(temp_pred)
    }
    v_tbl[1:length(pred_val), kval] <- pred_val
    rm(pred_val)
  }
  
  
  # print(paste("length of pred_val is: ", length(pred_val)))
  
  # the final comparison data frame
  vd <- data.frame()
  vd[1:length(s),1] <- s
  vd[,2] <- train_labels[s]
  colnames(vd) <- c("row", "actual")
  
  vd <- cbind(vd, v_tbl)
  
  return(vd)
}



