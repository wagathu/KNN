a<-matrix(c(1,2,2,3), nrow=2)
plot(a, type="o", xlim=c(0,4), ylim=c(0,4))
b<-matrix(c(0,0,1,2), nrow=2, byrow=T)
points(b, type="o", col="red")
c<-matrix(c(0,0,2,3), nrow=2, byrow=T)
points(c, type="o", col="red")
d<-matrix(c(1,2,1,3), nrow=2, byrow=T)
points(d, type="o", col="blue")
e<-matrix(c(0,0,1,3), nrow=2, byrow=T)
points(e, type="o", col="red")
f<-matrix(c(1,2,2,4), nrow=2, byrow=T)
points(f, type="o", col="green")


dist(a)
dist(d)
dist(f)


# trying some test data ---------------------------------------------------
library(dplyr)

dd<-select(wbcd, c("radius_se","radius_worst","diagnosis"))
dd<-dd[1:100,]
ddb<-filter(dd, diagnosis=="Benign");ddb<-ddb[,-3]
ddm<-filter(dd, diagnosis=="Malignant");ddm<-ddm[,-3]

ddlab<-dd$diagnosis
p<-select(wbcd, c("radius_se","radius_worst","diagnosis"))
p<-p[(sample(1:nrow(wbcd),1)), -3]


o1<-colMeans(ddb)
o2<-colMeans(ddm)

dd<-dd[,-3]
plot(dd, pch=20, col="red", ylim=c(0,30),xlim=c(0.0,2.0))
points(ddm, pch=20, col="blue")
points(p, col="black", pch="#")

dd<-dd[,-3] # removing the character column diagnosis

for (i in 1:nrow(dd))
{
  points(rbind(dd[i,],o), type="l", col="green")
}

tri_o1<-vector(length=nrow(dd)) # length of dd points to origin1
tri_o2<-vector(length=nrow(dd)) # length of dd points to origin2

tri_p<-vector(length=nrow(dd)) # length of dd points to point p
o_p1<-dist(rbind(p,o1)) # length of point p to origin1
o_p2<-dist(rbind(p,o2)) # length of point p to origin2

tri_a<-vector(length=nrow(dd)) # areas of all possible triangles


for (i in 1:nrow(dd))
{
  tri_o2[i]<-as.numeric(dist(rbind(dd[i,],o2)))
  tri_p[i]<-as.numeric(dist(rbind(dd[i,],p)))
  tri_a[i]<-areas(o_p2, tri_o2[i], tri_p[i])
}


# locate minimum in area vec ----------------------------------------------
locate_end<-function(x, param=c("max","min"))
{
  if (param=="min")
  {
    for (i in seq_along(x))
    {
      if (x[i]==min(x))
      {
        return(i)
      }
    }
  }
  else if (param=="max")
  {
    for (i in seq_along(x))
    {
      if (x[i]==max(x))
      {
        return(i)
      }
    }
  }
  else{stop}
}

m<-locate_end(tri_a, "min")
points(rbind(dd[m,],o1), type="l", col="black")
points(rbind(p,o1), type="l", col="black")
points(rbind(dd[m,],p), type="l", col="black")
       

areas<-function(x,y,z)
{ # x,y,z are sides of triangle
  s<-0.5*(x+y+z)
  return(sqrt(s*(s-x)*(s-y)*(s-z)))
}


