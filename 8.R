##############
##  QUIZ 8  ##
##############

library(Rfit)
college.data <- read.csv("C:/Users/mathstat/Downloads/Colleges.csv")

##  1  ##

# percent of applications accepted
college.data$appsaccperc <- (college.data$appsacc/college.data$appsrec)*100

##  2  ##

# percent of undergraduate who are full time
college.data$ftunderperc <- (college.data$ftunder/(college.data$ftunder+college.data$ptunder))*100

##  3  ##

ls.gradrate <- lm(gradrate~top10+facterm+sfratio+instpers+appsaccperc+ftunderperc, data = college.data)
summary(ls.gradrate)

##  4  ##

par(mfrow=c(1,2))
ls.gradrate.residuals <- rstudent(ls.gradrate)
ls.gradrate.fitted <- fitted.values(ls.gradrate)
plot(ls.gradrate.fitted, ls.gradrate.residuals, main = "Residual Plot\ngradrate~top10+facterm+sfratio+instpers+appsaccperc+ftunderperc", cex.main=0.75)
abline(h=c(-2,2))

qqnorm(ls.gradrate.residuals, main = "QQ-Plot\ngradrate~top10+facterm+sfratio+instpers+appsaccperc+ftunderperc", cex.main=0.75)
qqline(ls.gradrate.residuals)

##  5  ##

rank.gradrate <- rfit(gradrate~top10+facterm+sfratio+instpers+appsaccperc+ftunderperc, data = college.data)
summary(rank.gradrate, overall.test='drop')
rank.gradrate2 <- rfit(gradrate~top10+sfratio+instpers+appsaccperc+ftunderperc, data = college.data)
summary(rank.gradrate2, overall.test='drop')
rank.gradrate3 <- rfit(gradrate~top10+sfratio+instpers+ftunderperc, data = college.data)
summary(rank.gradrate3, overall.test='drop')

