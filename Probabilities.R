setwd("C:/Users/lucas/Desktop/Odd")
library(corpcor)
library(corrplot)
library(car)
library(ggplot2)
library(KernSmooth)
library(GGally)
library(corpcor)
library(corrplot)
library(car)
data <- read.table(file.choose(),header=T,sep=",")
head(data)
str(data)
attach(data)
fit1 <- lm(Mforskel~GnsMF+OH+OU+H+U)
fit2 <- lm(Mforskel~H+U)
fit1OddsH <- lm(OH~GnsMF+H+U)
fit1OddsU <- lm(OU~GnsMF+H+U)

summary(fit1)
summary(fit2)
summary(fit1OddsH)
summary(fit1OddsU)
fitted(fit1OddsH)[1]
fitted(fit1OddsU)[1]

drop1(fit1,test="F")
drop1(fit1OddsH,test="F")
model.matrix(fit1OddsH)
plot(fitted(fit1)~Mforskel)
plot(fitted(fit1OddsH)~OH)
plot(fitted(fit1OddsU)~OU)
plot(OH~GnsMF)




