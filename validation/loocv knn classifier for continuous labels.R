# knn leave one out function ----------------------------------------------

loocv_cknn <- function(train_dataset, train_lab, k_range)
{
  require(ModelMetrics)
  require(stringr)
  require(compiler)
  
  validation_train <- train_dataset
  v_tbl <- data.frame()
  
  ## loop thru the k range specified
  for (k_val in k_range)
  {
    message(paste("loocv training k at: ", k_val))
    ## loop thru number of train instances
    for (i in 1:nrow(train_dataset))
    {
      v_tbl[i, k_val] <- cknn_of(train=train_dataset[-i,], 
                             test=validation_train[i,], 
                             train_labs = train_lab[-i], 
                             k=k_val)
    }
  }
  View(v_tbl)
  
  # comparing using MSE
  val_vec <- vector()
  for (i in 1:ncol(v_tbl))
  {
    val_vec[i] <- mse(v_tbl[,i], train_lab)
  }
  names(val_vec) <- str_c("knn:",k_range)
  
  
  return(val_vec)
}


