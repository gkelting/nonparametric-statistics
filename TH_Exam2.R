################################
##  Nonparametric Statistics  ##
##      Take Home Exam 2      ##
################################

##  Grace Kelting
##  3969

set.seed(3969)
library(exactRankTests)

baseball.data <- read.csv("NLBB 2005.csv", header = TRUE, sep = ',', na.strings = "")

#################
##  PROBLEM 1  ##
#################

par(mfrow=c(1,1))
boxplot(Salary~InOut, data = baseball.data, main = "Salaries of Infielders vs. Outfielders", names = c("Infielders","Outfielders"), ylab = "Salary")

wilcox.exact(Salary~InOut, data = baseball.data)

#################
##  PROBLEM 2  ##
#################

baseball.data$OPS <- baseball.data$OBP + baseball.data$SLG

#################
##  PROBLEM 3  ##
#################

baseball.data$PA <- baseball.data$AB + baseball.data$BB + baseball.data$HBP + baseball.data$SH + baseball.data$SF

#################
##  PROBLEM 4  ##
#################

baseball.data$ADJ.AVG <- ifelse(baseball.data$PA < 502, (baseball.data$H)/(baseball.data$AB + (502 - baseball.data$PA)), baseball.data$AVG)

#################
##  PROBLEM 5  ##
#################

par(mfrow=c(3,2))

plot(x = baseball.data$AVG, y = baseball.data$Salary, main = "Salary vs. Batting Average", xlab = "Batting Average", ylab = "Salary")
#plot(x = rank(baseball.data$AVG), y = rank(baseball.data$Salary), main = "Ranks of Salary vs. Batting Average", xlab = "Batting Average", ylab = "Salary")
cor.test(baseball.data$AVG, baseball.data$Salary, method="kendall")

plot(x = baseball.data$OBP, y = baseball.data$Salary, main = "Salary vs. On-Base Percentage", xlab = "On-Base Percentage", ylab = "Salary")
#plot(x = rank(baseball.data$OBP), y = rank(baseball.data$Salary), main = "Ranks of Salary vs. On-Base Percentage", xlab = "On-Base Percentage", ylab = "Salary")
cor.test(baseball.data$OBP, baseball.data$Salary, method="kendall")

plot(x = baseball.data$SLG, y = baseball.data$Salary, main = "Salary vs. Slugging Percentage", xlab = "Slugging Percentage", ylab = "Salary")
#plot(x = rank(baseball.data$SLG), y = rank(baseball.data$Salary), main = "Ranks of Salary vs. Slugging Percentage", xlab = "Slugging Percentage", ylab = "Salary")
cor.test(baseball.data$SLG, baseball.data$Salary, method="kendall")

plot(x = baseball.data$OPS, y = baseball.data$Salary, main = "Salary vs. On-Base Plus Slugging", xlab = "On-Base Plus Slugging", ylab = "Salary")
#plot(x = rank(baseball.data$OPS), y = rank(baseball.data$Salary), main = "Ranks of Salary vs. On-Base Plus Slugging", xlab = "On-Base Plus Slugging", ylab = "Salary")
cor.test(baseball.data$OPS, baseball.data$Salary, method="kendall")

plot(x = baseball.data$ADJ.AVG, y = baseball.data$Salary, main = "Salary vs. Adjusted Average", xlab = "Adjusted Average", ylab = "Salary")
#plot(x = rank(baseball.data$ADJ.AVG), y = rank(baseball.data$Salary), main = "Ranks of Salary vs. Adjusted Average", xlab = "Adjusted Average", ylab = "Salary")
cor.test(baseball.data$ADJ.AVG, baseball.data$Salary, method="kendall")

#################
##  PROBLEM 6  ##
#################

baseball.data$AVG200 <- ifelse(baseball.data$AVG < 0.200, 0, 1)
baseball.data$ADJ.AVG200 <- ifelse(baseball.data$ADJ.AVG < 0.200, 0, 1)

#################
##  PROBLEM 7  ##
#################

chisq.test(baseball.data$Position,baseball.data$AVG200)
chisq.test(baseball.data$Position,baseball.data$AVG200)$residuals

chisq.test(baseball.data$Position,baseball.data$ADJ.AVG200)
fisher.test(baseball.data$Position,baseball.data$ADJ.AVG200)
fisher.test(baseball.data$Position,baseball.data$ADJ.AVG200, simulate.p.value = TRUE)
chisq.test(baseball.data$Position,baseball.data$ADJ.AVG200)$residuals

#################
##  PROBLEM 8  ##
#################

##  https://stackoverflow.com/questions/9126840/delete-rows-with-blank-values-in-one-particular-column/9127007
baseball.data.NOPITCH <- baseball.data[-which(baseball.data$Position == "Pitcher"),]

chisq.test(baseball.data.NOPITCH$Position,baseball.data.NOPITCH$AVG200)
fisher.test(baseball.data.NOPITCH$Position,baseball.data.NOPITCH$AVG200)
#chisq.test(baseball.data.NOPITCH$Position,baseball.data.NOPITCH$AVG200)$residuals

chisq.test(baseball.data.NOPITCH$Position,baseball.data.NOPITCH$ADJ.AVG200)
#chisq.test(baseball.data.NOPITCH$Position,baseball.data.NOPITCH$ADJ.AVG200)$residuals
