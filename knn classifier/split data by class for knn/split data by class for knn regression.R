

# Date: 11-1-2021
# Credits: stanley sayianka
# tags: ML, kNN, classifier, regression, MSE, 
# train and test split by categorical variables for knn classification

# train - the train dataset, and omit the target variable
# test -the test dataset, with the target variable ommited
# trainlabs - the train labels/train target variables
# k_value - the k value used for knn regression or classification
# cat - the index of the column containing categorical variables
#       by which to split the datasets

# knn for classification type
split_knn <- function(train, test, train_labs, k_value, cat)
{
  # libraries
  require(dplyr)
  
  # cleansing the labels
  # train_labs <- factor(train_labs)
  
  # retaining row names
  train <- cbind(rownames(train), train)
  test <- cbind(rownames(test), test)
  cat <- (cat+1)
  
  # print("check1")
  
  # creating split holders
  c <- unique(train[, cat])
  split_train <- list()
  split_test <- list()
  split_labs <- list()
  
  # print("check2")
  
  # splitting and removing the columns with splitter categories
  
  # use dplyr::select to avoid conflict with other functions named select
  c_name <- names(train[cat])
  for (i in 1:length(c))
  {
    split_train[[i]] <- filter(train, train[, cat] == c[i]) %>%
      dplyr::select(!all_of(c_name))
    
    split_test[[i]] <- filter(test, test[, cat] == c[i]) %>%
      dplyr::select(!all_of(c_name))
    
    split_labs[[i]] <- train_labs[as.integer(split_train[[i]][, 1])]
  }


  # print("check3")
  
  # performing knn
  # this is the part we now insert the knn function
  # (be it classification or regression)
  
  require(class)
  
  pred_labs <- list()
  for (i in 1:length(split_test))
  {
    # reducing k to number of rows: when k >= nrows(segment)
    temp_k <- k_value
    temp_k <- ifelse(k_value >= nrow(split_train[[i]]), 
                      yes = nrow(split_train[[i]]),
                      no = k_value)
    message(paste("Using inner k as: ", temp_k))
    
    # the continuous knn regression model
    pred_labs[[i]] <- cknn_of(train = split_train[[i]][, -1],
                              test = split_test[[i]][, -1],
                              train_labs = split_labs[[i]],
                              k = temp_k)
    rm(temp_k)
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

