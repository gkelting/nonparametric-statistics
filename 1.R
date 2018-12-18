####################
##  1.1-1.6 Quiz  ##
####################

thunder <- read.table("Thunder Stats.csv", TRUE, ",", row.names = "Players")

(rookie <- rep(c("N","R","N","R","N","R","N","R","N"), times=c(5,1,1,1,2,1,1,1,4)))

thunder$rookie <- rookie

tapply(thunder$MIN, thunder$rookie, mean)

(pts.game <- thunder$PTS/thunder$GP)

thunder$pts.game <- pts.game

thunder[,"PTS"]
max(thunder$PTS)
max(thunder$pts.game)





