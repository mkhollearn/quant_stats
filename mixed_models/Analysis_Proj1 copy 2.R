data <- read.csv("~/Desktop/SPRING 2022/Quant Stats II/Analysis project 1 - Mixed Models/OlympicPairs.txt", sep="", stringsAsFactors=TRUE)
View(data)

#loading necessary packages
install.packages("mlmRev")
library("lme4")
library("mlmRev")

#descriptives
library(pastecs)
stat.desc(data$IndPerformance)
stat.desc(data$IndMotivation)
stat.desc(data$DyadRelQuality)
stat.desc(data$DyadCohesion)
hist(data$IndPerformance)

#random intercept model
interceptonly  <- lmer(IndPerformance ~ (1 | DyadID),data= data, REML = FALSE)
summary(interceptonly)

#calculate the ICC
library(performance)
icc(interceptonly)

#one predictor mixed model
mixmodel1 <- lmer(IndPerformance ~ DyadRelQuality + (1 | DyadID), data = data, REML = FALSE)
summary(mixmodel1)
anova(interceptonly, mixmodel1, test = "Chisq") # calc likelihood ratio

#two predictor mixed model
mixmodel2 <- lmer(IndPerformance ~ DyadRelQuality + DyadCohesion + (1 | DyadID),data= data, REML = FALSE)
summary(mixmodel2)
anova(mixmodel1, mixmodel2, test = "Chisq")

#three predictor mixed model
mixmodel3 <- lmer(IndPerformance ~ DyadRelQuality + DyadCohesion + IndMotivation + (1 | DyadID),data= data, REML = FALSE)
summary(mixmodel3)
anova(mixmodel2, mixmodel3, test = "Chisq")







