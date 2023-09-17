
diagnose_kfold <- function(kfold_output)
{
  # k range used
  k_range <- ncol(kfold_output) - 2
  
  for (i in 3:ncol(kfold_output))
  {
    kfold_output[, i] <- ifelse(test = kfold_output[, i] == 1,
                       yes = 0,
                       no = 1)
  }
  
  total_acc <- vector()
  class_0 <- vector()
  class_1 <- vector()
  
  for (i in 3:ncol(kfold_output))
  {
    tab <- table(kfold_output[,2], kfold_output[,i])
    d <- diag(tab)
    rs <- rowSums(tab)
    
    total_acc <- c(total_acc, mean(kfold_output[,2]==kfold_output[,i]))
    class_0 <- c(class_0, (d/rs)[1]) %>%
      unname()
    class_1 <- c(class_1, (d/rs)[2]) %>%
      unname
  }
  
  plot(total_acc, type="o", col="blue", lwd=2, axes=F,
       main = "OVERALL ACCURACY")
  axis(1, at=1:20, labels=paste("k:",1:20))
  axis(2)
  box()
  
  plot(class_0, type="o", col="blue", lwd=2, axes=F,
       main = "CLASS:0 ACCURACY")
  axis(1, at=1:20, labels=paste("k:",1:20))
  axis(2)
  box()
  
  plot(class_1, type="o", col="blue", lwd=2, axes=F,
       main = "CLASS:1 ACCURACY")
  axis(1, at=1:20, labels=paste("k:",1:20))
  axis(2)
  box()
  
  par(mfrow=c(1,3))
  
  df_kfold <- cbind(total_acc, class_0, class_1) %>%
    data.frame()
}