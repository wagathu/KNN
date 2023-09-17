# k fold cross validation -------------------------------------------------

nn_kfold <- function(train_dataset, train_labels, fold, times, k_value)
{
  # fold - the k to use in k fold
  # times - the number of k batches to model on
  # k - The k to use in k nearest neighbours
  
  n <- nrow(train_dataset)
  
  # matrix to hold the k folds
  set.seed(328)
  s <- sample(n, (fold*times), replace = T)
  amat <- matrix(s, nrow=times, byrow=T)
  
  # to hold the values of the validations
  pred_val <- vector()
  
  
  for (i in 1:nrow(amat))
  {
    rand <- amat[i,]
    temp_pred <- knn(train=train_dataset[-rand,], 
                     test=train_dataset[rand,], 
                     cl=train_labels[-rand], 
                     k=k_value)
    
    pred_val <- c(pred_val, temp_pred)
    rm(temp_pred)
  }
  
  print(paste("length of pred_val is: ", length(pred_val)))
  
  # the final comparison data frame
  vd <- data.frame()
  vd[1:length(s),1] <- s
  vd[,2] <- train_labels[s]
  vd[,3] <- pred_val
  
  colnames(vd) <- c("row_instance", "actual_class", "pred_class")
  
  return(vd)
}



