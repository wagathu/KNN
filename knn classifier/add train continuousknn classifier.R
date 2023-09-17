### a custom made knn classifier

# original add train Date: 15-Nov-2020
# modified regression add train Date: 15-Nov-2020
# Credits: stanley sayianka
# tags: ML, kNN, classifier

add_trainKnn<-function(train, test, train_labs, k)
{
  # train - train dataset
  # test - test dataset
  # train_labs - train labels
  # k - the number of nearest neighbours
  
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
    
    # cls holds the winning label from the mean
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
        cls[i] <- mean(labels_holder[[i]])
      }
      
      train <- rbind(train, test[i, ])
      train_labs <- c(train_labs, cls)
    }
    
    
    message(paste("The train has rows: ", nrow(train)))
    message(paste("The test has rows: ", nrow(test)))
    
    #print(distance_list)

  }
  
  return(cls)
  #View(distance_holder)
}


