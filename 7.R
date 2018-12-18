##############
##  QUIZ 7  ##
##############

library(Rfit)

baseball.data <- read.csv("C:/Users/grace/OneDrive/Classes/NonParametric/Exams/Exam 2/NLBB 2005.csv", header = TRUE, sep = ',', na.strings = "")

##  1  ##
plot(x = baseball.data$AVG, y = baseball.data$Salary, main = "Salary vs. Batting Average", xlab = "Batting Average", ylab = "Salary")

##  2  ##
least.squares.regression <- lm(Salary~AVG, data = baseball.data)
summary(least.squares.regression)
abline(least.squares.regression)

##  3  ##
least.squares.residuals <- rstudent(least.squares.regression)
plot(x = baseball.data$AVG, y = least.squares.residuals, main="Residual Plot\nSalary vs. Batting Average")
abline(h=c(-2,2))

##  4  ##
qqnorm(least.squares.residuals, main = "QQ-PLot\nSalary vs. Batting Average")
qqline(least.squares.residuals)

##  5  ##
rank.based.regression <- rfit(Salary~AVG, data = baseball.data)
summary(rank.based.regression, overall.test = "drop")
plot(x = baseball.data$AVG, y = baseball.data$Salary, main = "Salary vs. Batting Average", xlab = "Batting Average", ylab = "Salary")
abline(least.squares.regression)
abline(rank.based.regression, lty = 2)
legend("topright", c("Least Squares", "Rank Based"), lty=c(1,2))