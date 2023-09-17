## analyzing knn-k values using a 
## "blackmarket" method of plot and table 

analyze_k<-function(train, test, train_labels, test_labels)
{ #train dataset, test dataset, train labels, test labels
  datasize<-nrow(test) # length of test observations
  predtable<-data.frame()
  predtable[1:datasize, 1]<-seq(1, datasize, 1)
  
  for (i in 1:datasize)
  { ### storing predicted columns
    predtable[, i]<-knn(train, test, train_labels, k=i)
  }
  
  a<-list() # a list of all crosstables
  for (i in 1:datasize)
  { ### storing crosstable lists for all values of k
    a[[i]]<-CrossTable(predtable[, i], test_labels, prop.chisq = F)
  }

  l<-length(unique(train_labels))
  sq<-seq(1, (l**2), l+1) # indexer sequence

  d_f<-data.frame() # stores percentage accuracy and errors
  d_f[1:datasize, 1]<-1:datasize
  
  for (i in 1:datasize)
  {    # storing percentage accuracy and errors
    d_f[i, 2]<-sum(as.data.frame(a[[i]][1])[sq, 3])
  }

  d_f[, 3]<-(d_f[, 2]/datasize)*100
  d_f[, 4]<-100 - d_f[, 3]
  colnames(d_f)<-c("k values", "index vector", "Percentage_accuracy", "Percentage_error")
  par(mfrow=c(1, 2))
  
  p<-plot(d_f[, 1], d_f[, 3], type="l", xlab="k values", col="blue", 
          ylab="Percentage Accuracy", main="PERCENTAGE ACCURACY PLOT", 
          ylim=c(10, 100), lwd=2)
  abline(h=max(d_f[, 3]), lty=3)
  grid()
  
  q<-plot(d_f[, 1], d_f[, 4], type="l", xlab="k values", col="red", 
          ylab="Percentage error", main="PERCENTAGE ERROR PLOT", ylim=c(0, 100), 
          lwd=2)
  abline(h=min(d_f[, 4]), lty=3)
  grid()
  
  return(View(d_f))
  
}
