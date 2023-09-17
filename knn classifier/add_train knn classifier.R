### a custom made knn classifier

# Date: 19-june-2020 to 24-june-2020
#       re edited on 7/7/2020 to make it fast
#       re edited to add vote function on 29th oct
# Credits: stanley sayianka
# tags: ML, kNN, classifier

vknn_of<-function(train, test, train_labs, k)
{
  # train - train dataset
  # test - test dataset
  # train_labs - train labels
  # k - the number of nearest neighbours
  
  # vote function for labels.
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
  
  
  if (ncol(train)!=ncol(test))
  {
    stop("Number of columns for both train and test dataset should be equal")
  }
  
  else
  {
    # distance holding
    # a dataframe to hold distances, col-test, row-train
    # the columns in distance holder are the test rows
    # the row names in distance holder are the train rows
    distance_holder<-data.frame()
    
    # list to hold ordered distances
    distance_list<-list()
    
    # a list to hold the nearest labels
    labels_holder<-list()
    
    # cls holds the winning label from the votes.
    cls <- vector()
    
    test<-as.matrix(test)
    train<-as.matrix(train)
    for (i in 1:nrow(test))
    {
      print(paste("Train now has rows: ", nrow(train)))
      for (j in 1:nrow(train))
      {
        #message(paste("training at: ", j))
        distance_holder[j,i]<-sqrt(sum((test[i, ]-train[j, ])**2)) # euclidean dist
      }
      
      
      # ordering of distances
      for (q in 1:ncol(distance_holder))
      {
        distance_list[[q]]<-order(distance_holder[, q])
      }
      
      # label holding
      for (i in 1:length(distance_list))
      {
        labels_holder[[i]]<-train_labs[distance_list[[i]][1:k]]
      }
      # print(labels_holder)
      
      
      for (i in 1:length(labels_holder))
      {
        cls[i] <- vote(labels_holder[[i]])
      }
      
      train <- rbind(train, test[i, ])
      train_labs <- c(train_labs, cls)
    }

    
    message(paste("The train has rows: ", nrow(train)))
    message(paste("The test has rows: ", nrow(test)))
    
    #print(distance_list)
    
    # getting k nearest neighbours
    #for (j in 1:length(distance_list))
    #{
    # print(train_labs[distance_list[[j]][1]])
    #}

  }
  
  return(cls)
  #View(distance_holder)
}


