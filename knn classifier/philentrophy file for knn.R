### a custom made knn classifier

# Date: 19-june-2020 to 24-june-2020
#       re edited on 7/7/2020 to make it fast
#       re edited to add vote function on 29th oct
# Credits: stanley sayianka
# tags: ML, kNN, classifier

nn_of<-function(train, test, train_labs, k, 
                distance=c("euclidean","manhattan", "pnorm", "chebyshev",
                           "cosine-similarity", ""),
                p = c(NULL, p))
{
  # train - train dataset
  # test - test dataset
  # train_labs - train labels
  # k - the number of nearest neighbours
  # d - the distance formulae to be used
  # p - the parameter to be used in case of choosing pnorm distance
  
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
          z[j,i] <- suppressMessages(philentropy::distance(rbind(x[i,],y[j,]), 
                                          method="euclidean")) %>%
            as.numeric()
        }
      }
      return(abs(z)) # omitting negative signs
    }
    
    distance_holder<-data.frame()
    # a dataframe to hold distances, col-test, row-train
    # the columns in distance holder are the test rows
    # the row names in distance holder are the train rows
    distance_holder<-dhfn(test,train)

    distance_list<-list()
    # a list to hold the orders of distances
    for (q in 1:ncol(distance_holder))
    {
      distance_list[[q]]<-order(distance_holder[, q])
    }
    
    # a list to hold the nearest labels
    labels_holder<-list()
    for (i in 1:length(distance_list))
    {
      labels_holder[[i]]<-train_labs[distance_list[[i]][1:k]]
    }
    
    # vote function
    vote <- function(x)
    {
      # x is a vector
      u <- unique(x)
      
      # a vector to store instances of occurences
      ll <- rep(0, length(u))
      
      for (j in 1:length(u))
      {
        for (i in 1:length(x))
        {
          if (u[j] == x[i])
          {
            ll[j] <- ll[j]+1
          }
        }
      }
      
      for (i in 1:length(ll))
      {
        if (ll[i] == max(ll))
        {
          winner <- i
        }
        else{}
      }
      # print(u)
      # print(ll)
      return(u[winner])
    }
    
    cls <- vector()
    for (i in 1:length(labels_holder))
    {
      cls[i] <- vote(labels_holder[[i]])
    }
    
    
  }
  
  return(cls)
}


