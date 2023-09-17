### a custom made knn classifier, so slow due to loops

# Date: 19-june-2020 to 24-june-2020
# Credits: stanley sayianka
# tags: ML, kNN, classifier

sknn<-function(train, test, train_labs, k)
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
    
    
    dhfn<-function(x,y)
    { ## distance holder function
      #x,y are MATRICES
      #x-test
      #y-train
      z<-data.frame()
      x<-as.matrix(x)
      y<-as.matrix(y)
      for (i in 1:nrow(x))
      {
        for (j in 1:nrow(y))
        {
          z[j,i]<-sqrt(sum((x[i, ]-y[j, ])**2)) # euclidean dist
          # z[j,i]<-sum(sqrt((x[i, ]-y[j, ])**2)), manhattan dist
        }
      }
      return(z)
    }
    
    distance_holder<-data.frame()
    distance_holder<-dhfn(test,train)
    
    distance_list<-list()
    # a list to hold the orders of distances
    for (q in 1:ncol(distance_holder))
    {
      distance_list[[q]]<-order(distance_holder[, q])
    }
    #print(distance_list)
    
    # getting k nearest neighbours
    #for (j in 1:length(distance_list))
    #{
    # print(train_labs[distance_list[[j]][1]])
    #}
    
    
    # a list to hold the nearest labels
    labels_holder<-list()
    for (i in 1:length(distance_list))
    {
      labels_holder[[i]]<-train_labs[distance_list[[i]][1:k]]
    }
    # print(labels_holder)
    
    
    # a list to hold max no. of each k vector
    holder_of_k_max<-list()
    for (i in 1:length(labels_holder))
    {
      holder_of_k_max[[i]]<-max(table(labels_holder[[i]]))
    }
    # print(holder_of_k_max)
    
    # list to hold unique elements
    holder_unique_class<-list()
    for (i in 1:length(labels_holder))
    {
      holder_unique_class[[i]]<-unique(labels_holder[[i]])
    }
    # print(holder_unique_class)
    
    # a list to hold true values of factor k
    predicted_class<-list()
    for (i in 1:length(labels_holder))
    {
      for (j in 1:length(unique(labels_holder[[i]])))
      {
        if (length(labels_holder[[i]][labels_holder[[i]]==holder_unique_class[[i]][j]])==holder_of_k_max[[i]])
        {
          predicted_class[[i]]<-holder_unique_class[[i]][j]
        }
      }
    }
    
  }
  
  return(predicted_class)
  #View(distance_holder)
}


