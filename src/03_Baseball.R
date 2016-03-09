#Import Libraries
library(ggplot2)
library(dplyr)
library(reshape2)

#Read Dataset
baseball = read.csv("Baseball.csv")

# Understand data struture and summarize it
str(baseball)
summary(baseball)

#Q1
Year=baseball$Year
Number.of.Teams=baseball$NumCompetitors
baseball1=data.frame(Year,Number.of.Teams)
baseball1=unique(baseball1)
ggplot(baseball1,aes(x=as.factor(Year),y=Number.of.Teams)) + geom_bar(stat="identity",color="black",fill="blue")

#Creating a logistic regression models for each variable in a bivariate model
#Logistic Model for Year and is significant
#AIC = 232.35
WS1 = glm(WonWorldSeries ~ Year, data = baseball, family = binomial)
summary(WS1)

#Logistic Model for Runs Scored
#AIC = 241.45
WS2 = glm(WonWorldSeries ~ RS, data = baseball, family = binomial)
summary(WS2)

#Logistic Model for Runs Allowed and is significant
#AIC = 237.88
WS3 = glm(WonWorldSeries ~ RA, data = baseball, family = binomial)
summary(WS3)

#Logistic Model for Wins
#AIC = 239.51
WS4 = glm(WonWorldSeries ~ W, data = baseball, family = binomial)
summary(WS4)

#Logistic Model for On Base Percentage
#AIC = 242.02
WS5 = glm(WonWorldSeries ~ OBP, data = baseball, family = binomial)
summary(WS5)

#Logistic Model for Slugging Percentage
#AIC = 239.23
WS6 = glm(WonWorldSeries ~ SLG, data = baseball, family = binomial)
summary(WS6)

#Logistic Model for Batting Average
#AIC = 243.08
WS7 = glm(WonWorldSeries ~ BA, data = baseball, family = binomial)
summary(WS7)

#Logistic Model for Rank of their Season and is significant
#AIC = 238.75
WS8 = glm(WonWorldSeries ~ RankSeason, data = baseball, family = binomial)
summary(WS8)

#Logistic Model for Number of Competitors and is significant
#AIC = 230.96
WS9 = glm(WonWorldSeries ~ NumCompetitors, data = baseball, family = binomial)
summary(WS9)

#Logistic Model for League
#AIC = 242.88
WS10 = glm(WonWorldSeries ~ League, data = baseball, family = binomial)
summary(WS10)

#Logistic Model using the significant variables
WS = glm(WonWorldSeries ~ Year + RA + RankSeason + NumCompetitors, data = baseball, family = binomial)
summary(WS)


#Logistic Model using each variable
model1 = glm(WonWorldSeries ~ Year + RS + W + OBP + SLG + BA + League + RA + RankSeason + NumCompetitors, data = baseball, family = binomial)
summary(model1)

#Determine which is the best model to use
step(model1, direction = "both")


#Obtain correlations between independent variables to suggest why
#some independent variables are not significant in bivariate model
baseball1 = baseball
baseball1$League = NULL
baseball1$Team = NULL
qplot(x=Var1,y=Var2, data=melt(cor(baseball1)), fill=value, geom="tile") + geom_text(aes(Var2,Var1,label=round(value,digits =2)),color="white",size=5)
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
pairs(baseball, lower.panel=panel.smooth)
pairs(baseball, lower.panel = panel.smooth, upper.panel = panel.cor)
cor(baseball)
pairs(baseball, lower.panel = panel.smooth, upper.panel = panel.cor)
