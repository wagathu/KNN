

# experimenting manhattan and chebyshev dist
# creating dummy codes
aa <- matrix(c(1,0,0,1,1,1,0,0), ncol=2)

distance(aa, method = "chebyshev")
# manhattan is binary distance

col <- colors()[1:10]
coin <- c("HEAD", "TAIL")
res <- c("WIN", "FAIL", "HOLD")

df <- cbind(sample(col, 10, replace = T),
            sample(coin, 10, replace = T),
            sample(res, 10, replace = T))
View(df)

onehot_restruct <- function(data)
{
  # the one hot encode function for single vectors
  # one hot encode
  onehot_swap<-function(x)
  {
    df<-data.frame()
    ux<-unique(x)
    
    for (i in 1:length(unique(x)))
    {
      for (j in 1:length(x))
      {
        if (ux[i]==x[j])
        {
          df[j,i]<-1
        }
        else
        {
          df[j,i]<-0
        }
      }
    }
    names(df)<-unique(x)
    return(df)
  }
  
  biglist <- apply(data, 2, onehot_swap)
  fd <- biglist[[1]]
  
  for (i in 2:length(biglist))
  {
    fd <- cbind(fd, biglist[[i]])
  }
  return(fd)
}

distance(ans, method = "manhattan")
distance(ans, method = "chebyshev")