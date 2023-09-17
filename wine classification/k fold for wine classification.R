kdd <- nn_kfold(wq_train, wq_train_lab, fold=673, times=20, k_range = 75)

dim(kdd)
View(kdd)


# adding a 3 will make the predicted labels correct
for (i in 3:ncol(kdd))
{
  kdd[,i] <- kdd[,i]+2
}


## comparing using tables
tbl_validation <- list()

for (i in 3:ncol(kdd))
{
  tbl_validation[[i]] <- table(kdd[,i], kdd[,2])
}

val_tibble <- data.frame()

# use k range as used in k fold function here
val_tibble[1:75,1] <- 1:75

## calculating accuracy
for (i in 1:length(tbl_validation))
{
  val_tibble[i,2] <- (sum(diag(tbl_validation[[i]])))/length(kdd[,2])*100
}
colnames(val_tibble) <- c("k_value", "accuracy")
val_tibble <- na.omit(val_tibble)

library(ggplot2)
library(gganimate)
library(plotly)

p <- ggplot(data=val_tibble, aes(x=k_value, y=accuracy))+
  geom_line(col="maroon", lwd=1)+
  labs(title = "ACCURACY OF K VALUES:", x="k values", y="percentage accuracy")


p + transition_reveal(k_value)

ggplotly(p)


# instead of using 1 best k we can take a vote on all k's in a certain range

vv <- vector()
for (i in 1:nrow(kdd))
{
  #tvec <- kdd[i, 3:ncol(kdd)] %>% 
  #  as.vector()
  vv[i] <- vote(kdd[i, 3:ncol(kdd)])
  print(i)
}
vv <- unlist(vv)



tt <- table(vv,kdd$actual)
(sum(diag(tt)))/length(kdd[,2])*100

# choosing only the best performing row combinations of k's ----------------------


posk <- c(3,4,39,36,34,35,41) # the best k's
pp <- kdd[, (posk+2)]
vv <- vector()

for (i in 1:nrow(pp))
{
  tvec <- pp[i,] %>% 
    as.vector()
  
  vv[i] <- vote(tvec)
  
}
vv <- unlist(vv)

table(vv,kdd$actual)



# performing the k fold techniques on the test dataset --------------------

t_pred <- data.frame()

for (i in 1:length(posk))
{
  t_pred[1:nrow(wq_test),i] <- knn(train=wq_train, test=wq_test, 
                    cl=wq_train_lab, k=posk[i], prob=T)
}

vv <- vector()
for (i in 1:nrow(t_pred))
{
  temp <- t_pred[i,] %>%
    as.vector()
  vv[i] <- vote(temp)  
}

vv <- unlist(vv)

table(vv, wq_test_lab)
as <- table(vv, wq_test_lab); sum(diag(as))/sum(as)

# performing the k fold train techniques on test dataset --------------------

k_test <- test_kfold(train_dataset=wq_train, test_dataset=wq_test, train_labels=wq_train_lab, fold=673, times=20, k_range=1:75)
