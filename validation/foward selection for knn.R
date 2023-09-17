
# foward and backward selection for classification ------------------------

knn_fwd <- function(train_data, test_data, train_labs, test_labs, k_neighbours)
{
  require(class)
  
  # splitting df into test and train
  #n <- round(nrow(df)*test_percentage)
  #rand <- sample(1:nrow(df), n, replace = F)
  #df_train <- df[-rand, ]
  #df_test <- df[rand, ]
  
  #train_labs <- labs[-rand]
  #test_labs <- labs[rand]
  
  # accuracy vector to store the most accurate k
  acc_vec <- vector()
  
  # the model starts at 2 because euclidean distance
  # cant calculate one vector distances
  
  for (i in 2:ncol(train_data))
  {
    #print(paste("USING COLUMNS: ",2," TO", i))
    
    temp_pred <- knn(train = train_data[, 1:i], test = test_data[, 1:i], 
                     cl=train_labs, k=k_neighbours)
    
    acc_vec[i] <- sum(diag(table(temp_pred, test_labs)))/length(test_labs)
    tb <- table(temp_pred, test_labs)
    print(tb)
    # message(paste("Benign Accuracy: ", (tb[,1][1])/sum(tb[,1])))
    # message(paste("Malignant Accuracy: ", (tb[,2][2])/sum(tb[,2])))
    
    
    
    # testing for accuracy drop
    if (i>=3 && acc_vec[i] < acc_vec[i-1])
    {
      message(paste("Adding column: ",i, "reduces overall accuracy by: ",
                    acc_vec[i-1] - acc_vec[i]))
      # message(paste("Benign Accuracy: ", (tb[,1][1])/sum(tb[,1])))
      # message(paste("Malignant Accuracy: ", (tb[,2][2])/sum(tb[,2])))
    }
    
  }
  
  # a simple plot
  plot(x=1:ncol(train_data), y=acc_vec, 
       col="blue", type="l", lwd=2, 
       xlab="Columns used", ylab="Percentage accuracy achieved")
  axis(1, at=seq(1, ncol(train_data), 1))
  
  return(acc_vec)
  
}

ob <- c()
om <- c()
