#################
##  2.7a Quiz  ##
#################

set.seed(3969)

#O, A, B, AB
bloodtype.frequency <- c(67, 83, 29, 8)
bloodtype.dist <- c(0.45, 0.4, 0.11, 0.04)

(bloodtype.chisq <- chisq.test(bloodtype.frequency, p = bloodtype.dist))

bloodtype.chisq$residuals