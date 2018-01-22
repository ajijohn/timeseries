### get accidental death data ...

filter.with.padding <- function(x,the.filter,iter=1)
{
  q <- (length(the.filter)-1)/2
  n <- length(x)
  w <- filter(c(rep(x[1],q),x,rep(x[n],q)),the.filter)[(q+1):(q+n)]
  if(iter > 1) for(i in 2:iter) w <- filter(c(rep(w[1],q),w,rep(w[n],q)),the.filter)[(q+1):(q+n)]
  return(w)
}
ad.ts <- scan("http://faculty.washington.edu/dbp/s519/Data/deaths.txt")/1000
ad.years <- seq(1973,1979-1/12,1/12)

### overhead III-83

plot(ad.years,ad.ts,col="blue",xlab="year",typ="b",ylab=expression(paste(x[t]," (thousands)")),main="Monthly Counts of Accidental Deaths in USA",cex=0.5)

### overhead III-85

m.hat <- filter.with.padding(ad.ts,c(1/24,rep(1/12,11),1/24))

plot(ad.years,ad.ts,col="blue",xlab="year",typ="b",ylab=expression(paste(x[t]," and ", hat(m)[t]," (thousands)")),main="Monthly Counts of Accidental Deaths in USA",cex=0.5)
lines(ad.years,m.hat,col="red",lwd=2)
