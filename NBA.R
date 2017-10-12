# Load libraries into RStudio

library(dplyr)
library(magrittr)
library(ggplot2)

# Read data

NBA_train <- read.csv("C:/Users/narce/OneDrive/Documents/GitHub/NBA/NBA/NBA_train.csv")

NBA <-NBA_train

# Examine the structure of the data (835 observations and 20 variables)

str(NBA)

# Variables definitions

# SeasonEnd: Year the season ended
# Team: Name of team
# Playoffs: Binary variable for playoff appearance
# W: Wins (regular season) 
# PTS: Points scored (regular season)
# oppPTS:Oopponent points scored (regular season)
# FG, FGA: Field goals (including three pointers)
# X2P, X2PA: 2-pointers
# X3P, X3PA: 3-pointers
# FT, FTA: Free throws
# ORB, DRB: Offensive and defensive rebounds
# AST: Assists
# STL: Steals
# BLK: Blocks
# TOV: Turnovers

# The goal here is to determine how many regular season wins are needed to make the playoffs. 
# I start by grouping the data by the number of wins and 
# creating new features: number of playoff appearances, 
# and the percentage of playoff appearances fracPO divided by total number of wins.

tmp<- group_by(NBA, W) %>% summarise(nTot = n(), nPO = sum(Playoffs), fracPO = nPO/nTot)

View(tmp)

# Here I plot the number of Wins on the x axis and playoff appearance on the y axis. 
# This plot shows that (45>) wins have a (90>) chance of making it to the playoffs.

plot(tmp$W, tmp$fracPO, pch = 21, col = "red2", bg = "orange", 
     xlab = "Wins", ylab = "Making the Playoffs", main = "Playoff Appearance / Regular Season Wins")
abline(h = 0.9, lty = 3, col = "red2")
abline(v = 35, lty = 3, col = "blue2")
abline(v = 45, lty = 3, col = "blue2")


# Predicting wins by calculating the difference between points scored (PTS) and points allowed (oppPTS)

NBA$PTSdiff <- NBA$PTS - NBA$oppPTS

# Check the linear relationship between PTSdiff and Wins

plot(NBA$PTSdiff, NBA$W, pch = 21, col = "red2", bg = "orange",
     xlab = "PTS Difference", ylab = "Wins", main = "Wins / Points Scored")

# This is the linear regression Model for wins - PTSdiff = independent variable and W = dependent variable

Wins_Reg <- lm(W ~ PTSdiff, data = NBA)

summary(Wins_Reg)

W = 41 + 0.0325*(NBA$PTSdiff)

# The linear regression Model computes the PTSdiff needed attain W=> 45, PTSdiff>122.8

# This formula will predict points scored. Dependent variable = PTS, independent variable = (X2PA, X3PA, FTA, ORB, DRB, AST, ST, BL, TOV)


PointsRS <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + TOV + STL + BLK, data = NBA)

summary(PointsRS)

# The summary shows significant correlation between X2PA, X3PA, FTA, AST, ORB and Wins (each having 3 stars).
# Independent variables DRB, TOV, STL, BLK do not show a strong correlation to determining Win
# R^ value = 0.8992 showing a good linear relationship between the independent and dependent variables to points scored.

# Sum of Squared Errors

SSE <- sum(PointsRS$residuals^2)
print(SSE)

# Root Mean Error

RMSE <- sqrt(SSE/PointsRS$df.residual)
RMSE

mean(NBA$PTS)

# Fractional error = 2.2%  


#-------------
# It may be interesting to check the correlations between the variables 
# that we included in this first Model, to get some hints as to collinearity, 
# which could be relevant to know if we wanted to remove some variables.

#par(mar=c(5, 4, 4, 1)+0.1)
#par(oma=c(0, 0, 0, 0))
#pairs(NBA[, c("X2PA", "X3PA", "FTA", "AST", "ORB", "DRB", "TOV", "STL", "BLK")], gap=0.5, las=1,
#      pch=21, bg=rgb(0,0,1,0.25),
#      panel=mypanel, lower.panel=function(...) panel.cor(..., color.bg=TRUE), main="")
#mtext(side=3, "pairs plot with correlation values", outer=TRUE, line=-1.2, font=2)
#mtext(side=3, "Dashed lines are 'lm(y~x)' fits.\nCorrelation and scatterplot frames are color-coded on the str 
#length of the correlation",
#      outer=TRUE, line=-1.6, padj=1, cex=0.8, font=1)
#--------------  


# Running Model #2 removing TOV

PointsRS2 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + DRB + STL + BLK, data = NBA)

summary(PointsRS2)

# By removing TOV R^2^ = 0.8991 which is higher than Model1 at 0.8992.
# In Model3 DRB is removed

PointsRS3 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL + BLK, data = NBA)
summary(PointsRS3)

# The Model3 R^2^ is the same at Model1  at 0.8991

# In Model4 BLK is removed

PointsRS4 <- lm(PTS ~ X2PA + X3PA + FTA + AST + ORB + STL, data = NBA)
summary(PointsRS4)

# The Model4 R^2^ is the same at Model1 and Model3 at 0.8991
# A closer look at SSE and RMSE of Model4

SSE_4 <-sum(PointsRS4$residuals^2)
RSME_4 <- sqrt(SSE_4/nrow(NBA))

# The values for Model4 (PointsRS4) are SSE = 28421464.9 and RMSE = 184.493 compared the the Model1 RMSE = 185.5191 (essentially the same)

# Time for predictions
# Read in NBA_test data

NBA_test <- read.csv("C:/Users/narce/OneDrive/Documents/GitHub/NBA/NBA/NBA_test.csv")

# Attempting to predict using Model4, the number of points in the 
# 2012-2013 season using predict() and the new NBA_test.csv data

PointsPredictions <- predict(PointsRS4, newdata = NBA_test)
summary(PointsPredictions)

# To determine the accuracy of Model4 (0.8991) the SSE, SST, RMSE must be analyzed

SSE <- sum((PointsPredictions - NBA_test$PTS)^2)
SSE
SST <- sum((mean(NBA$PTS) - NBA_test$PTS)^2)

# R2 is calculated with 1 minus the sum of squared errors divided by the total sum of squares

R2 <- 1 - SSE/SST
R2

RMSE <- sqrt(SSE/nrow(NBA_test))

RMSE

# The values for Model4 (PointsRS4) are SSE = 28421464.9 and RMSE =  compared the the Model1 RMSE = 185.5191 (essentially the same)
# The predictions: 
# Model4 RMSE = 184.493 vs NBA_test = 196.37
# Model4 SSE = 28421464 vs NBA_test = 1079739
# R2 = 0.8127

