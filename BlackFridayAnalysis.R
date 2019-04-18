setwd("~/Emilio/DS")
BFDataset<- read.csv("BlackFriday.csv",  skip =0,  
             comment.char = "",check.names = FALSE, quote="",
             na.strings=c("NA","NaN", " ") )

summary(BFDataset)
str(BFDataset)

 #install.packages("tidyverse")
library(tidyverse)
 
library(ggplot2)
ggplot(BFDataset, aes(x = Purchase)) +
  geom_histogram()

model1 <- lm(Purchase ~ Gender + Age + Occupation + City_Category + Stay_In_Current_City_Years + Marital_Status + Product_Category_1 + Product_Category_2 + Product_Category_3,data = BFDataset)
summary(model1)

model2 <- lm(Purchase ~ Gender + Age + Occupation + City_Category + Stay_In_Current_City_Years  + Product_Category_1 + Product_Category_2 + Product_Category_3,data = BFDataset)
summary(model2)

test

