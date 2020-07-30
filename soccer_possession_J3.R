# 決定木
View(soccer_possession_J3)
head(soccer_possession_J3)
library(rpart)
library(rpart.plot)
J3.rp <- 
  rpart(
    formula = Pts ~ Shoot_Rate + Possession,
    data = soccer_possession_J3,
    method = 'anova',
    control = rpart.control(minsplit = 5, cp= .1)
  )
summary(J3.rp)

J3.rp$variable.importance
rpart.plot(J3.rp)

# 重回帰
m <- lm(Pts ~ Shoot_Rate + Possession, data = soccer_possession_J3)
summary(m)


plot(soccer_possession_J3$Shoot_Rate,soccer_possession_J3$Pts)
abline(lm(soccer_possession_J3$Pts~soccer_possession_J3$Shoot_Rate), col="red") # regression line (y~x)
lines(lowess(soccer_possession_J3$Pts~soccer_possession_J3$Shoot_Rate), col="blue") # lowess line (x,y)


plot(soccer_possession_J3$Possession,soccer_possession_J3$Pts)
abline(lm(soccer_possession_J3$Possession,soccer_possession_J3$Pts), col="red") # regression line (y~x)
lines(lowess(soccer_possession_J3$Possession,soccer_possession_J3$Pts), col="blue") # lowess line (x,y)


pairs(~Possession+Pts+GF+GA,data=soccer_possession_J3,main="Simple Scatterplot Matrix")

library(psych)
pairs.panels(soccer_possession_J3[c( "Pts", "Possession", "GF", "GA")])

soccer_possession_J3
m <- lm(GA ~ Possession, data = soccer_possession_J3)
summary(m)
