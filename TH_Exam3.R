################################
##  Nonparametric Statistics  ##
##      Take Home Exam 3      ##
################################

##  Grace Kelting
##  3969

#################
##  PROBLEM 1  ##
#################

testing.normal.tests <- function(n, mu.g1, mu.g2, mu.g3){
  
  library(coin)
  library(DescTools)
  
  mu <- rep(c("Group 1", "Group 2", "Group 3"), times = rep(n, 3))
  mu.factor <- factor(mu)
  mu.ordered <- ordered(mu.factor)
  
  ANOVA.reject <- rep(0, times = 10000)
  KW.reject <- rep(0, times = 10000)
  JT.reject <- rep(0, times = 10000)
  
  for (i in 1:10000){
    sample1 <- rnorm(n, mean = mu.g1, sd = 1)
    sample2 <- rnorm(n, mean = mu.g2, sd = 1)
    sample3 <- rnorm(n, mean = mu.g3, sd = 1)
    samples <- c(sample1, sample2, sample3)
    
    lm.samples <- lm(samples~mu.factor)
    ANOVA <- anova(lm.samples)
    ANOVA.pvalue <- ANOVA$`Pr(>F)`[1]
    if(ANOVA.pvalue < 0.05){ANOVA.reject[i] = 1}
    
    KW <- kruskal_test(samples~mu.factor)
    KW.pvalue <- pvalue(KW)[1]
    if(KW.pvalue < 0.05){KW.reject[i] = 1}
    
    JT <- JonckheereTerpstraTest(samples, mu.ordered)
    JT.pvalue <- JT$p.value
    if(JT.pvalue < 0.05){JT.reject[i] = 1}
  }
  
  ANOVA.power <- mean(ANOVA.reject)
  KW.power <- mean(KW.reject)
  JT.power <- mean(JT.reject)
  
  if (mu.g1 == mu.g2 & mu.g2 == mu.g3 & mu.g3 == 1){
    list("ANOVA Type 1 Error" = ANOVA.power, "Kruskal-Wallis Type 1 Error" = KW.power, "Jonckheere-Terpstra Type 1 Error" = JT.power)
  }
  else{
    list("ANOVA Power" = ANOVA.power, "Kruskal-Wallis Power" = KW.power, "Jonckheere-Terpstra Power" = JT.power)
  }
  
}

set.seed(3969)
testing.normal.tests(n = 10, mu.g1 = 0.5, mu.g2 = 1, mu.g3 = 1.25)

#################
##  PROBLEM 2  ##
#################

set.seed(3969)
testing.normal.tests(n = 10, mu.g1 = 1, mu.g2 = 1, mu.g3 = 1)

#################
##  PROBLEM 3  ##
#################

testing.exponential.tests <- function(n, mu.g1, mu.g2, mu.g3){
  
  library(coin)
  library(DescTools)
  
  mu <- rep(c("Group 1", "Group 2", "Group 3"), times = rep(n, 3))
  mu.factor <- factor(mu)
  mu.ordered <- ordered(mu.factor)
  
  ANOVA.reject <- rep(0, times = 10000)
  KW.reject <- rep(0, times = 10000)
  JT.reject <- rep(0, times = 10000)
  
  for (i in 1:10000){
    sample1 <- rexp(n, rate = 1/mu.g1)
    sample2 <- rexp(n, rate = 1/mu.g2)
    sample3 <- rexp(n, rate = 1/mu.g3)
    samples <- c(sample1, sample2, sample3)
    
    lm.samples <- lm(samples~mu.factor)
    ANOVA <- anova(lm.samples)
    ANOVA.pvalue <- ANOVA$`Pr(>F)`[1]
    if(ANOVA.pvalue < 0.05){ANOVA.reject[i] = 1}
    
    KW <- kruskal_test(samples~mu.factor)
    KW.pvalue <- pvalue(KW)[1]
    if(KW.pvalue < 0.05){KW.reject[i] = 1}
    
    JT <- JonckheereTerpstraTest(samples, mu.ordered)
    JT.pvalue <- JT$p.value
    if(JT.pvalue < 0.05){JT.reject[i] = 1}
  }
  
  ANOVA.power <- mean(ANOVA.reject)
  KW.power <- mean(KW.reject)
  JT.power <- mean(JT.reject)
  
  if (mu.g1 == mu.g2 & mu.g2 == mu.g3 & mu.g3 == 1){
    list("ANOVA Type 1 Error" = ANOVA.power, "Kruskal-Wallis Type 1 Error" = KW.power, "Jonckheere-Terpstra Type 1 Error" = JT.power)
  }
  else{
    list("ANOVA Power" = ANOVA.power, "Kruskal-Wallis Power" = KW.power, "Jonckheere-Terpstra Power" = JT.power)
  }
}

set.seed(3969)
testing.exponential.tests(n = 10, mu.g1 = 0.5, mu.g2 = 1, mu.g3 = 1.25)

#################
##  PROBLEM 4  ##
#################

set.seed(3969)
testing.exponential.tests(n = 10, mu.g1 = 1, mu.g2 = 1, mu.g3 = 1)