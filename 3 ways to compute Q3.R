##  HOW TO FIND Q3 DIFFERENT WAYS
gp <- matrix(c(43, 45, 53, 56, 56, 57, 58, 66, 67, 73, 74, 79, 80, 80, 81, 
               81, 81, 82, 83, 83, 84, 88, 89, 91, 91, 92, 92, 97, 99, 99, 
               100, 100, 101, 102, 102, 102, 103, 104, 107, 108, 109, 113,
               114, 118, 121, 123, 126, 128, 137, 138, 139, 144, 145, 147, 
               156, 162, 174, 178, 179, 184, 191, 198, 236, 239, 268, 274, 
               379, 430, 478, 661, 672, 748))

set.seed(9897)

gp.boot <- matrix(sample(gp, 10000*72, replace=TRUE), nrow = 10000, ncol=72)


##  1
gp.boot.75 <- vector()
for(i in 1:nrow(gp.boot)){
  upper.quant <- quantile(gp.boot[i,], 0.75)
  gp.boot.75 <- c(gp.boot.75, upper.quant)
}
sort(gp.boot.75)
quantile(gp.boot.75, probs=c(0.005, 0.995), type=1)

##  2
Q3 <- function(data){
  q3 <- quantile(data, 0.75)
}
gp.boot.75 <- apply(gp.boot, 1, Q3)
quantile(gp.boot.75, probs=c(0.005, 0.995), type=1)

##  3
upper.quart <- function(data){
  # sort the data
  sorted <- sort(data)
  n <- length(sorted)
  # want 75th percentile
  p <- 0.75
  # third quartile position
  third.quart.pos <- ((n-1)*p) + 1
  f <- floor(third.quart.pos)
  d <- third.quart.pos - f
  third.quart <- sorted[f] + d*(sorted[f+1]-sorted[f])
  #https://avrilomics.blogspot.com/2013/05/calculating-quantiles-using-r.html
}

gp.boot.75 <- apply(gp.boot, 1, upper.quart)
quantile(gp.boot.75, probs=c(0.005, 0.995), type=1)

##  4

quartstuff.boot <- function(data,i){
  q <- data[i]
  qq <- sort(q)
  #print(length(qq))
  qq[54]
}

bootstuff <- apply(gp.boot, 1, quartstuff.boot)
quantile(bootstuff, probs=c(0.005, 0.995), type=1)

#install.packages("boot")
library(boot)
set.seed(9897)
bootthing <- boot(data = gp, statistic=quartstuff.boot, R=10000)
boot.ci(bootthing, conf=0.99)


sorted <- sort(gp)
med <- median(gp)
upper <- sorted[sorted > med]
medu <- median(upper)

summary(gp)


