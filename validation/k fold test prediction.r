# k fold cross validation -------------------------------------------------

test_kfold <- function(train_dataset, test_dataset, train_labels, fold, times, k_range)
{
  # fold - the k to use in k fold
  # times - the number of k batches to model on
  # k - The k to use in k nearest neighbours
  
  n <- nrow(train_dataset)
  
  # matrix to hold the k folds
  set.seed(3872)
  s <- sample(x=n, size=(fold*times), replace = T)
  amat <- matrix(s, nrow=times, byrow=T)
  rm(s) # removing s to free space
  
  #print(paste("Dimensions of amat are: ", dim(amat)))

  # to hold the values of the validations
  
  v_tbl <- array(dim=c(nrow(test_dataset), length(k_range), times))
  
  for (k in 1:length(k_range))
  {
    kval <- k_range[k]
    print(paste("Time: ", Sys.time(),"Using k as: ", kval))
    pred_val <- vector()
    
    for (i in 1:nrow(amat))
    {
      rand <- amat[i,]
      v_tbl[1:nrow(test_dataset), kval, i] <- knn(train=train_dataset[-rand,], 
                       test=test_dataset, 
                       cl=train_labels[-rand], 
                       k=kval)

    }
  }
  # print(paste("length of pred_val is: ", length(pred_val)))
  
  # the final comparison data frame
  return(v_tbl)
}



