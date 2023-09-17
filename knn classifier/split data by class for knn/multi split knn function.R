

# Date: 11-1-2021
# Credits: stanley sayianka
# tags: ML, kNN, classifier, regression, MSE, 
# train and test split by categorical variables for knn classification


# knn for classification type
multisplit_knn <- function(train, test, train_labs, k_value, cat)
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
  # creating the unique list holder
  c_unique <- list()
  for (i in 1:length(cat))
  {
    c_unique[[i]] <- unique(train[, cat[i]])
  }
  
  u_dd <- data.frame()
  for (i in 1:length(c_unique))
  {
    
  }
  
  # creating split holders
  c <- unique(train[, cat])
  split_train <- list()
  split_test <- list()
  split_labs <- list()
  
   print("check2")
  
  # splitting and removing the columns with splitter categories
  
  # use dplyr::select to avoid conflict with other functions named select
  c_name <- names(train[cat])
  for (i in 1:length(c))
  {
    split_train[[i]] <- dplyr::filter(train, train[, cat] == as.character(c[i])) %>%
      dplyr::select(!all_of(c_name))
    
    split_test[[i]] <- dplyr::filter(test, test[, cat] == as.character(c[i])) %>%
      dplyr::select(!all_of(c_name))
    
    split_labs[[i]] <- train_labs[as.integer(split_train[[i]][, 1])]
  }
  
  
   print("check3")
  
  # performing knn
  # this is the part we now insert the knn function
  # (be it classification or regression)
  
  require(class)
  
  pred_labs <- list()
  for (i in 1:length(split_test))
  {
    # reducing k to number of rows
    k_value <- ifelse(k_value >= nrow(split_train[[i]]), 
                      yes = nrow(split_train[[i]]),
                      no = k_value)
    message(paste("Using inner k as: ", k_value))
    # the continuous knn regression model
    pred_labs[[i]] <- knn(train = split_train[[i]][, -1],
                              test = split_test[[i]][, -1],
                              cl = split_labs[[i]],
                              k=k_value)
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

tr <- newdd_train[, 1:5]
te <- newdd_test[, 1:5]
trl <- newdd_train[, 6]

ww <- split_knn(train=tr, 
                test = te, 
                train_labs = trl, 
                k_value = 5, 
                cat=3)

for (i in 1:10)
{
  message(paste("Using k as: ", i))
  
  ww <- split_knn(train=tr, 
                  test = te, 
                  train_labs = trl, 
                  k_value = i, 
                  cat=3)
  
  print(mse(newdd_test$`Y house price of unit area`, ww[, 2]))
}
