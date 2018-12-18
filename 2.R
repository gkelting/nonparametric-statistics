####################
##  1.7-1.8 Quiz  ##
####################

set.seed(3969)

# 10000 samples of size 30
chisq.data <- matrix(rchisq(n = 10000 * 30, df = 1), nrow = 10000, ncol = 30)

# perform t test on the chi square distribution and strip p-value
ttest.pval <- function (data, H0){
  t.test(data, mu = H0, alternative = "two.sided")$p.value
}

# apply ttest.pval to each sample in the chi square distribution
# samples are housed in the rows, thus 1
# in chi square, H0 = mean = df, thus H0 = 1
pval <- apply(chisq.data, 1, ttest.pval, H0 = 1)

# portion of the p values less than 0.05
# What is the proportion of p-values less than 0.05 for H0 = 1?
(power <- mean(pval < 0.05))

# multiple null hypotheses need to be tested
# added H0 = 1 here for convenience when creating scatterplot
null.hypotheses <- c(1, 1.25, 1.5, 1.75, 2, 3, 4)

# initialized vector of powers from the null hypotheses
mult.powers <- vector() 

# for convenience, I created a for loop
for(i in null.hypotheses){
  pval <- apply(chisq.data, 1, ttest.pval, H0 = i)
  power <- mean(pval < 0.05)
  mult.powers <- c(mult.powers, power)
}

# scatter plot of powers vs. null hypotheses
plot(x = null.hypotheses, y = mult.powers, type = "b", xlab = "Null Hypotheses", ylab = "Power of Null Hypothesis", main = "Powers of Chi Square Distribution vs. Different Null Hypotheses")




