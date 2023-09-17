# knn leave one out function ----------------------------------------------

knn_validation <- function(train_dataset, train_lab, k_range)
{
  validation_train <- train_dataset
  v_tbl <- data.frame()
  
  ## loop thru the k range specified
  for (k_val in k_range)
  {
    ## loop thru number of train instances
    for (i in 1:nrow(train_dataset))
    {
      v_tbl[i, k_val] <- knn(train=train_dataset[-i,], 
                             test=validation_train[i,], 
                             cl=train_lab[-i], 
                             k=k_val, 
                             prob=T)
    }
  }
  # dim(v_tbl)
  return(v_tbl)
  ## comparing using tables
  tbl_validation <- list()
  
  for (i in 1:ncol(v_tbl))
  {
    tbl_validation[[i]] <- table(v_tbl[,i], train_lab)
  }
  
  val_tibble <- data.frame()
  val_tibble[k_range,1] <- k_range
  
  ## calculating accuracy
  for (i in 1:length(tbl_validation))
  {
    val_tibble[i,2] <- (sum(diag(tbl_validation[[i]])))/length(train_lab)*100
  }
  colnames(val_tibble) <- c("k_value", "accuracy")
  
  #library(ggplot2)
  #library(gganimate)
  #library(plotly)
  
  #p <- ggplot(data=val_tibble, aes(x=k_value, y=accuracy))+
  #  geom_line(col="maroon", lwd=1.2)+
  #  labs(title = "ACCURACY OF K VALUES:", x="k values", y="percentage accuracy")
  
  #p + theme_classic()  + transition_reveal(k_value)
  #ggplotly(p)
  
  return(val_tibble)
}


