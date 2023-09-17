diagnose_k <- function(train, test, train_labs, test_labs, k_value)
{
  dhfn<-function(x,y)
  { ## distance holder function
    #x,y are MATRICES
    #x-test data
    #y-train data
    z<-data.frame()
    x<-as.matrix(x)
    y<-as.matrix(y)
    for (i in 1:nrow(x))
    {
      for (j in 1:nrow(y))
      {
        z[j,i]<-sqrt(sum((x[i, ]-y[j, ])**2)) # euclidean dist
      }
    }
    return(z)
  }
  
  pred <- knn(train = train, test=test, cl=train_labs, k=k_value)
  
  # agreement function
  agreement <- function(actual, predicted)
  {
    disagreement_vec <- vector()
    for (i in 1:length(actual))
    {
      if (actual[i] != predicted[i])
      {
        disagreement_vec[i] <- i
        message(paste("Disagreement at :", i))
      }
      else{}
    }
    return(disagreement_vec %>% na.omit() %>% as.integer())
  }
  
  d.a_vec <- agreement(actual = test_labs, predicted = pred)
  
  distance_holder<-data.frame()
  # a dataframe to hold distances, col-test, row-train
  # the columns in distance holder are the test rows
  # the row names in distance holder are the train rows
  distance_holder<-dhfn(test,train)
  
  #View(distance_holder)
  # looking at the rowsshowing minimum distances
  min_rows <- list()
  for (i in 1:length(d.a_vec))
  {
    min_rows[[i]] <- rownames(distance_holder[order(distance_holder[,i]),])[1:k_value]
  }
  print(min_rows)
}

diagnose_k(train=wbcd_train, test=wbcd_test,
           train_labs = wbcd_train_labels, test_labs = wbcd_test_labels,
           k_value = 5)
