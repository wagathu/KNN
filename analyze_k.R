## "table-plot" method of analyzing range of 
## k values for k nearest neighbours

analyze_k<-function(train, test, train_labels, test_labels, k_range)
{ 
  # train - the training dataset
  #test - the test dataset
  #train_labels
  #test_labels
  #k_range - the number of k values to 
  #           use(should be numeric and greater than 0)
  
  #if (class(k_range) != "numeric")
  #{
  #  print("The k range is not numeric and does not hold a numeric figure")
  #}
  require(class)
  
  prediction_table<-data.frame() # to store predicted classes
  prediction_table[1:nrow(test), 1]<-seq(1, nrow(test), 1)
  
  for (i in 1:k_range)
  { ### storing predicted-class columns
    prediction_table[, i]<-knn(train, test, train_labels, k=i)
  }
  
  tab_list<-list() # a list of all tables comparing actual and predicted classes
  for (i in 1:k_range)
  { ### storing crosstable lists for all values of k
    tab_list[[i]]<-table(prediction_table[, i], test_labels)
  }
  
  l<-length(unique(train_labels))
  sq<-seq(1, (l**2), l+1) # indexer sequence
  
  d_f<-data.frame() # stores percentage accuracy and errors
  d_f[1:k_range, 1]<-1:k_range
  
  for (i in 1:k_range)
  {    # storing percentage accuracy and errors
    d_f[i, 2]<-sum(tab_list[[i]][sq])
  }
  
  d_f[, 3]<-(d_f[, 2]/nrow(test))*100
  d_f[, 4]<-100 - d_f[, 3]
  #d_f[, 5]<-ifelse(max(d_f[, 3]), "*", ".") ## the star(*)-represents highest accuracy
                                            ## the dot(.) represents just any other value


  colnames(d_f)<-c("k values", "index vector", "Percentage_accuracy", "Percentage_error")
  par(bg="black",mfrow=c(1, 2))
  
  plot(d_f[, 1], d_f[, 3], type="l", xlab="k values", 
       col="blue", 
       ylab="Percentage Accuracy", main="ACCURACY PLOT", 
       col.main="white", 
       ylim=c((min(d_f[, 3])-5), 100), lwd=2, 
       col.axis="azure3", col.lab="azure3")
  
  abline(h=max(d_f[, 3]), lty=1)
  grid(,lty=1, col="wheat4")
  
  plot(d_f[, 1], d_f[, 4], type="l", xlab="k values", 
       col="red", 
       ylab="Percentage error", main="ERROR PLOT",
       col.main="white", 
       ylim=c(0, (max(d_f[, 4]+5))), 
       lwd=2, col.axis="azure3", col.lab="azure3")
  
  abline(h=min(d_f[, 4]), lty=1)
  grid(,lty=1, col="wheat4")
  
  analyze_k_table<-d_f[-2]
  return(analyze_k_table)
}

