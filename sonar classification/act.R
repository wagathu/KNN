pbinom(7,20,0.2)
sum(dbinom(0:7, 20, 0.2))

pnbinom(8,4,0.3)
dnbinom(x=4,size=4,prob=0.3)

ppois(7,lambda=5)
sum(dpois(0:7, 5))

pgeom(6,0.01)
?pgeom

pnorm(3.9925)-pnorm(3.737)

solve_lambda<-function(x)
{ ## trial and error to find a lambda for a poisson
    for (i in 1:x)
    {
        l<-rnorm(1)
        if (round(l/(exp(l)-1), 2)==0.4)
        {
            return(l)
        }
    }
}

solve_lambda(1000)

