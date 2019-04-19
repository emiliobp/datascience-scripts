#setwd("/Users/emilio/Documents/Data Science")
trainDf <- read.csv("./BlackFriday.csv", header = TRUE, na.strings=c("NA","#DIV/0!",""))

str(trainDf)

#require(leaps)
#mejores_modelos <- regsubsets(Purchase~., data = trainDf, nvmax =11 )
# El argumento nvmax determina el tamaño máximo de los modelos a inspeccionar.
# Si se quiere realizar best subset selection evaluando todos los posibles 
# modelos, nvmax tiene que ser igual al número de variables disponibles
library(tidyverse)

ggplot(data = trainDf, mapping = aes(x = Age, y = Purchase)) + geom_point()

ggplot(data = trainDf, mapping = aes(x = Age, y = Purchase)) + geom_boxplot()
ggplot(data = trainDf, mapping = aes(x = Gender, y = Purchase)) + geom_boxplot()

ggplot(data = trainDf, mapping = aes(x = Age, y = Purchase, color = Gender)) +
  geom_boxplot() +
  facet_wrap(~ Occupation)
head(trainDf, 4)
#sales = b0 + b1*youtube + b2*facebook + b3*newspaper
model <- lm(Purchase ~ Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years+Marital_Status, data = trainDf)
summary(model)

summary(model)$coefficient

model <- lm(Purchase ~ Gender+Age+Occupation+City_Category+Stay_In_Current_City_Years, data = trainDf)
summary(model)

confint(model)

