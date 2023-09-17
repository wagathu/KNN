

# Date: 11-1-2021
# Credits: stanley sayianka
# tags: ML, kNN, classifier, regression, MSE, 
# train and test split by categorical variables for knn classification


# knn for classification type
split_knn <- function(train, test, train_labs, k_value, cat)
{
  # libraries
  require(dplyr)
  
  # cleansing the labels
  train_labs <- factor(train_labs)
  
  # retaining row names
  train <- cbind(rownames(train), train)
  test <- cbind(rownames(test), test)
  cat <- (cat+1)
  
  print("check1")
  
  # creating split holders
  c <- unique(train[, cat])
  split_train <- list()
  split_test <- list()
  split_labs <- list()
  
  print("check2")
  
  # splitting and removing the columns with splitter categories
  c_name <- names(train[cat])
  for (i in 1:length(c))
  {
    split_train[[i]] <- filter(train, train[, cat] == c[i]) %>%
      select(!all_of(c_name))
    
    split_test[[i]] <- filter(test, test[, cat] == c[i]) %>%
      select(!all_of(c_name))
    
    split_labs[[i]] <- train_labs[train_labs == c[i]]
  }
  
  # performing knn
  require(class)
  
  pred_labs <- list()
  for (i in 1:length(split_test))
  {
    pred_labs[[i]] <- knn(train = split_train[[i]][, -1],
                          test = split_test[[i]][, -1],
                          cl = split_labs[[i]],
                          k=k_value, 
                          prob = F)
  }
  
  final_labs <- unlist(pred_labs)
  
  # re arranging rownames of test
  # i had to write a function which behaves like
  # stringi::stri_join_list but for dataframes.
  str_join_df <- function(df)
  {
    # df is a list of dataframes
    l <- length(df)
    final <- data.frame()
    
    for (i in 1:length(df))
    {
      final <- rbind(final, df[[i]])
    }
    return(final)
  }
  
  test_df <- str_join_df(split_test)
  
  # the rownames for the test dataset and labels
  r_names_test <- test_df[, 1] %>%
    as.integer()
  
  final_pred <- cbind(r_names_test, final_labs)
  names(final_pred) <- c("rownames", "pred_labs")
  
  # ordering in order to have proper rownames
  final_pred <- final_pred[order(final_pred[, 1]), ]
  
  
  return(final_pred)
}



