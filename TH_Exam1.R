################################
##  Nonparametric Statistics  ##
##      Take Home Exam 1      ##
################################

##  Grace Kelting
##  3969

#install.packages("coin")
library(coin) #my home computer likes this one
#install.packages("exactRankTests")
library(exactRankTests) #my laptop likes this one

##  lifetime (in days) of 72 guinea pigs

gp <- matrix(c(43, 45, 53, 56, 56, 57, 58, 66, 67, 73, 74, 79, 80, 80, 81, 
               81, 81, 82, 83, 83, 84, 88, 89, 91, 91, 92, 92, 97, 99, 99, 
               100, 100, 101, 102, 102, 102, 103, 104, 107, 108, 109, 113,
               114, 118, 121, 123, 126, 128, 137, 138, 139, 144, 145, 147, 
               156, 162, 174, 178, 179, 184, 191, 198, 236, 239, 268, 274, 
               379, 430, 478, 661, 672, 748))


#################
##  PROBLEM 1  ##
#################

##  t Test
gp.ttest <- t.test(gp, alternative = "two.sided", conf.int=TRUE, conf.level = 0.99)
(gp.ttest)

##  confidence interval
(gp.ttest$conf.int)

#################
##  PROBLEM 2  ##
#################

##  Wilcoxen Exact test
gp.wexact <- wilcox.exact(gp, alternative = "two.sided", exact = TRUE, conf.int=TRUE, conf.level = 0.99)
(gp.wexact)

##  confidence interval
(gp.wexact$conf.int)

#################
##  PROBLEM 3  ##
#################

set.seed(3969)

gp.boot <- matrix(sample(gp, 10000*72, replace=TRUE), nrow = 10000, ncol=72)

gp.boot.med <- apply(gp.boot, 1, median)

quantile(gp.boot.med, probs=c(0.005, 0.995), type=1)

#################
##  PROBLEM 4  ##
#################

##  histogram
hist(gp, xlab = "Lifetime (in days)", main = "Histogram of Guinea Pig Lifetimes")

##  qq plot
qqnorm(gp, main = "Normal Q-Q Plot of Guinea Pig Lifetimes")
qqline(gp)

#################
##  PROBLEM 6  ##
#################

##  75th percentile
##  IE all the numbers below the one in the 75% position

Q3 <- function(data){
  q3 <- quantile(data, 0.75)
}

gp.boot.75 <- apply(gp.boot, 1, Q3)

quantile(gp.boot.75, probs=c(0.005, 0.995), type=1)