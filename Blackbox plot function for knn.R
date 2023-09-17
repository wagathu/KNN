## knn blackmarket method of plots

knn_Pred<-function(train, test, train_lab, test_lab)
{
  datasize<-nrow(test) # length of test observations
  per_acc<-data.frame()
  per_acc[1:datasize, 1]<-seq(1, datasize, 1)
  for (i in 1:datasize)
  { ### storing predicted columns
    per_acc[, i]<-knn(train, test, train_lab, k=i)
  }
  a<-list()
  for (i in 1:datasize)
  { ### storing crosstable lists for all values of k
    a[[i]]<-CrossTable(per_acc[, i], test_lab, prop.chisq = F)
  }
  per_acc_digit<-data.frame()
  per_acc_digit[1:datasize, 1]<-1:datasize
  
  l<-length(unique(test_lab))
  size_seq<-function(l) 
  { ### function to find exact no. of each unique factor level
    sq<-seq(1, (l**2), l+1)
    return(sq) ### sq is a vector of locations
  }
  
  d_f<-data.frame()
  d_f[1:datasize, 1]<-1:datasize

  for (i in 1:datasize)
  {    
    d_f[i, 2]<-sum(as.data.frame(a[[i]][1])[sq, 3])
  }
  ######## the cut values
  
  d_f[, 3]<-(d_f[, 2]/datasize)*100
  d_f[, 4]<-100 - d_f[, 3]
  colnames(d_f)<-c("k values", "index vector", "Percentage_accuracy", "Percentage_error")
  par(mfrow=c(1, 2))
  p<-plot(d_f[, 3], type="l", xlab="Percentage accuracy", col="blue", 
          ylab="k values", main="PERCENTAGE ACCURACY PLOT")
  grid(10)
  q<-plot(d_f[, 4], type="l", xlab="Percentage error", col="red", 
               ylab="k values", main="PERCENTAGE ERROR PLOT")
  grid(10)
  return(View(d_f))
  
}
