### a custom made knn classifier

# Date: 19-june-2020 to 24-june-2020
#       re edited on 7/7/2020 to make it fast
#       re edited to add vote function on 29th oct
# Credits: stanley sayianka
# tags: ML, kNN, classifier

nn_of<-function(train, test, train_labs, k, 
                distance=c("euclidean","manhattan", "pnorm", "chebyshev",
                           "cosine-similarity", "hassanat"),
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
          if (distance =="euclidean")
          {
            # euclidean dist or pnorm w/ p=2 dist
            z[j,i] <- sqrt(sum((x[i, ]-y[j, ])**2)) 
          }
          else if(distance =="manhattan")
          {
            # manhattan dist, or a pnorm w/ p=1 dist
            z[j,i] <- (sum(abs(x[i, ]-y[j, ]))) 
          }
          else if(distance =="pnorm")
          {
            # pnorm dist/minkowski distance, w/ parameter p
            z[j,i] <- (sum(abs(x[i, ]-y[j, ]))**p)**(1/p) 
          }
          else if(distance == "chebyshev")
          {
            # chebyshev dist or pnorm dist w/ p=infinity
            z[j,i] <- (max(abs(x[i, ]-y[j, ]))) 
          }
          else if (distance == "cosine-similarity")
          {
            # applying the norm function and dot product
            # it is basically finding cos (theta)
            z[j,i] <- (sum(x[i, ]*y[j, ]))/
              (base::norm(as.matrix(x[i, ]), type="f") * base::norm(as.matrix(y[j, ]), type="f"))
          }
          else if (distance == "hassanat")
          {
            minval <- vector()
            maxval <- vector()
            d <- vector()
            
            amat <- cbind(as.numeric(x[i, ]), as.numeric(y[j, ]))
            for (a in 1:nrow(amat))
            {
              minval[a] <- min(amat[a,])
              maxval[a] <- max(amat[a,])
              
              d[a] <- ifelse(
                test = minval[a] >= 0,
                yes = (1-((1+minval[a])/
                            (1+maxval[a]))),
                no = (1-((1+minval[a]+abs(minval[a]))/
                           (1+maxval[a]+abs(minval[a]))))
              )
              
            }
            z[j, i] <- sum(d)
          }
          else
          {
            # the default: euclidean dist
            z[j,i]<-sqrt(sum((x[i, ]-y[j, ])**2)) 
            
          }
        }
      }
      return(z)
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

  
