### a custom made knn classifier for regression analysis

# Date: 22-Nov-2020
# Credits: stanley sayianka
# tags: ML, kNN, classifier, regression, MSE

cknn_of<-function(train, test, train_labs, k)
{
  require(compiler)
  # train - train dataset
  # test - test dataset
  # train_labs - train labels(continuous labels for regression)
  # k - the number of nearest neighbours
  
  if (ncol(train)!=ncol(test))
  {
    stop("Number of columns for both train and test dataset should be equal")
  }
  
  else
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
    # speeding up with compiler
    # cmpdhfn <- cmpfun(dhfn)
    
    distance_holder<-data.frame()
    # a dataframe to hold distances, col-test, row-train
    # the columns in distance holder are the test rows
    # the row names in distance holder are the train rows
    distance_holder<-dhfn(test,train)
    
    
    distance_list<-list()
    # a list to hold the orders of distances
    
    # a list to hold the nearest labels
    labels_holder<-list()
    cls <- vector()
    
    for (i in 1:ncol(distance_holder))
    {
      distance_list[[i]]<-order(distance_holder[, i])
      
      #labels_holder[[i]]<-train_labs[distance_list[[i]][1:k]]
      #cls[i] <- mean(labels_holder[[i]])
      cls[i] <- mean(train_labs[distance_list[[i]][1:k]])
    }
    #print(distance_list)
    
    #print(labels_holder)
  }
  
  # return(cls)
  return(cls)
  #View(distance_holder)
}
