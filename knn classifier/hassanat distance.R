
ls()

amat = cbind(x,y)

library(philentropy)
philentropy::distance(amat[1:2,], method="kullback-leibler", unit="log") %>%
  as.numeric()

class(xx)

nn_of(amat[1:5,],amat[6:10,],distance = "euclidean")

pp <- nn_of(train = wbcd_train, test = wbcd_test, train_labs = wbcd_train_labels,
            k = 21)



u <- c(2)
v <- c(5) 
dist(rbind(u,v))

sum((u-v)**2)

# hassanat distance
hassanat <- function(x,y)
{
  minval <- vector()
  maxval <- vector()
  d <- vector()
  
  amat <- cbind(u,v)
  for (i in 1:nrow(amat))
  {
    minval[i] <- min(amat[i,])
    maxval[i] <- max(amat[i,])
    
    d[i] <- ifelse(
      test = minval[i] >= 0,
      yes = (1-((1+minval[i])/
                  (1+maxval[i]))),
      no = (1-((1+minval[i]+abs(minval[i]))/
                 (1+maxval[i]+abs(minval[i]))))
    )

  }
  return(sum(d))
}

u=c(5.1, 3.5, 1.4, 0.3)
v=c(5.4, 3.4, 1.7, 0.2)

d <- philentropy::getDistMethods()
ve <- vector()
for (i in 1:length(d))
{
  message(d[i])
  suppressMessages(ve[i] <- distance(rbind(u,v), method=d[i], p=3, unit = "log"))
  print(a)
}


