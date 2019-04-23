#*******Example Regression to the Mean*******
library(UsingR)
data(father.son)
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
library(ggplot2)
g = ggplot(data.frame(x, y), aes(x = x, y = y))
g = g + geom_point(size = 5, alpha = .2, colour = "black")
g = g + geom_point(size = 4, alpha = .2, colour = "red")
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(position = "identity")
g = g + geom_abline(intercept = 0, slope = rho, size = 2)
g = g + geom_abline(intercept = 0, slope = 1 / rho, size = 2)
g = g + xlab("Father's height, normalized")
g = g + ylab("Son's height, normalized")
g

#*******Single Parameter Regression*******
library(UsingR); 
data(diamond)
y = diamond$price; 
x = diamond$carat
mean(y)
#[1] 500.0833
#using least squares
coef(lm(y ~ 1))

#*******Using Regression for Prediction*******
library(UsingR)
data(diamond)
library(ggplot2)
g = ggplot(diamond, aes(x = carat, y = price))
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 7, colour = "black", alpha=0.5)
g = g + geom_point(size = 5, colour = "blue", alpha=0.2)
g = g + geom_smooth(method = "lm", colour = "black")
g

fit <- lm(price ~ carat, data = diamond)
coef(fit)
#(Intercept) carat
#-259.6 3721.0

fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
coef(fit2)
#(Intercept) I(carat - mean(carat))
#500.1 3721.0

fit3 <- lm(price ~ I(carat * 10), data = diamond)
coef(fit3)
#(Intercept) I(carat * 10)
#-259.6 372.1

newx <- c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit)[2] * newx
#[1] 335.7 745.1 1005.5

predict(fit, newdata = data.frame(carat = newx))
#1 2 3
#335.7 745.1 1005.5

#The code below shows how to obtain the residuals.
data(diamond)
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
## The easiest way to get the residuals
e <- resid(fit)
## Obtain the residuals manually, get the predicted Ys first
yhat <- predict(fit)
## The residuals are y - yhat. Let's check by comparing this
## with R's build in resid function
max(abs(e -(y - yhat)))
#[1] 9.486e-13
## Let's do it again hard coding the calculation of Yhat
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))
#[1] 9.486e-13