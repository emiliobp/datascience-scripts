#How to compute regression with categorical variables.

#Categorical variables (also known as factor or qualitative variables) are variables that classify observations into groups. 
#They have a limited number of different values, called levels. For example the gender of individuals are a categorical variable that can take two levels: Male or Female.

#Regression analysis requires numerical variables. So, when a researcher wishes to include a categorical variable in a regression model, 
#supplementary steps are required to make the results interpretable.

#In these steps, the categorical variables are recoded into a set of separate binary variables. 
#This recoding is called “dummy coding” and leads to the creation of a table called contrast matrix. 
#This is done automatically by statistical software, such as R.
#http://www.sthda.com/english/articles/40-regression-analysis/163-regression-with-categorical-variables-dummy-coding-essentials-in-r/

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


##Finding residual variance estimates.
y <- diamond$price; 
x <- diamond$carat; 
n <- length(y)
fit <- lm(y ~ x)
## the estimate from lm
summary(fit)$sigma
#[1] 31.84
## directly calculating from the residuals
sqrt(sum(resid(fit)^2) / (n - 2))
#[1] 31.84

##
library(ggplot2)
newx = data.frame(x = seq(min(x), max(x), length = 100))
p1 = data.frame(predict(fit, newdata= newx,interval = ("confidence")))
p2 = data.frame(predict(fit, newdata = newx,interval = ("prediction")))
p1$interval = "confidence"
p2$interval = "prediction"
p1$x = newx$x
p2$x = newx$x
dat = rbind(p1, p2)
names(dat)[1] = "y"
g = ggplot(dat, aes(x = x, y = y))
g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2)
g = g + geom_line()
g = g + geom_point(data = data.frame(x = x, y=y), aes(x = x, y = y), size = 4)
g


##Multivariate example
require(datasets); data(swiss); ?swiss
summary(lm(Fertility ~ . , data = swiss))

##Other example
require(datasets);data(InsectSprays); require(stats); require(ggplot2)
g = ggplot(data = InsectSprays, aes(y = count, x = spray, fill = spray))
g = g + geom_violin(colour = "black", size = 2)
g = g + xlab("Type of spray") + ylab("Insect count")
g

summary(lm(count ~ spray, data = InsectSprays))$coef

# Therefore, 0.8333 is the estimated mean comparing Spray B to Spray A (as B - A), -12.4167 compares
# Spray C to Spray A (as C - A) and so on. The inferential statistics: standard errors, t value and P-value
# all correspond to those comparisons. The intercept, 14.5, is the mean for Spray A. So, its inferential
# statistics are testing whether or not the mean for Spray A is zero. As is often the case, this test isn’t
# terribly informative and often yields extremely small statistics (since we know the spray kills some
# bugs). The estimated mean for Spray B is its effect plus the intercept (14.5 + 0.8333); the estimated
# mean for Spray C is 14.5 - 12.4167 (its effect plus the intercept) and so on for the rest of the factor
# levels.

#Let’s hard code the factor levels so we can directly see what’s going on. Remember, we simply leave
#out the dummy variable for the reference level.
summary(lm(count ~
               I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +
               I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
               I(1 * (spray == 'F'))
             , data = InsectSprays))$coef

#Of course, it’s identical. You might further ask yourself, what would happen if I included a dummy
#variable for Spray A? Would the world implode? No, it just realizes that one of the dummy variables
#is redundant and drops it.

summary(lm(count ~
               I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +
               I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
               I(1 * (spray == 'F')) + I(1 * (spray == 'A')), data = InsectSprays))$coef

#However, if we drop the intercept, then the Spray A term is no longer redundant. Then each
#coefficient is the mean for that Spray.
summary(lm(count ~ spray - 1, data = InsectSprays))$coef

# So, for example, 14.5 is the mean for Spray A (as we already knew), 15.33 is the mean for Spray B 
# (14.5+ 0.8333 from our previous model formulation), 2.083 is the mean for Spray C (14.5 - 12.4167 from
# our previous model formulation) and so on. This is a nice trick if you want your model formulated
# in the terms of the group means, rather than the group comparisons relative to the reference group.
# Also, if there are no other covariates, the estimated coefficients for this model are exactly the
# empirical means of the groups. We can use dplyr to check this really easily and grab the mean
# for each group.

library(dplyr)
summarise(group_by(InsectSprays, spray), mn = mean(count))


# Often your lowest alphanumeric level isn’t the level that you’re most interested in as a reference
# group. There’s an easy fix for that with factor variables; use the relevel function. Here we give a
# simple example. We created a variable spray2 that has Spray C as the reference level.
spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data = InsectSprays))$coef

# Now the intercept is the mean for Spray C and all of the coefficients are interpreted with respect to
# Spray C. So, 12.417 is the comparison between Spray A and Spray C (as A - C) and so on.

# Summary of dummy variables
# If you haven’t seen this before, it might seem rather strange. However, it’s essential to understand
# how dummy variables are treated, as otherwise huge interpretation errors can be made. Here we
# give a brief bullet summary of dummy variables to help solidify this information.

# • If we treat a variable as a factor, R includes an intercept and omits the alphabetically first level
# of the factor.
# – The intercept is the estimated mean for the reference level.
# – The intercept t-test tests for whether or not the mean for the reference level is 0.
# – All other t-tests are for comparisons of the other levels versus the reference level.
# – Other group means are obtained the intercept plus their coefficient.
# 
# • If we omit an intercept, then it includes terms for all levels of the factor.
# – Group means are now the coefficients.
# – Tests are tests of whether the groups are different than zero.
# • If we want comparisons between two levels, neither of which is the reference level, we could
# refit the model with one of them as the reference level.



#*******Further analysis of the swiss dataset*******
library(datasets); data(swiss)
head(swiss)

library(dplyr)
swiss = mutate(swiss, CatholicBin = 1 * (Catholic > 50))

#Since we’re interested in Agriculture as a variable and Fertility as an outcome, let’s plot those two
#color coded by the binary Catholic variable:
require(ggplot2)
g = ggplot(swiss, aes(x = Agriculture, y = Fertility, colour = factor(CatholicBin)))
g = g + geom_point(size = 6, colour = "black") + geom_point(size = 4)
g = g + xlab("% in Agriculture") + ylab("Fertility")
g

#Let’s first fit the model with Xi2 removed.

summary(lm(Fertility ~ Agriculture, data = swiss))$coef

#*******Residuals*******

data(swiss); par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss); plot(fit)

#*******Multivariable regression*******
# Parsimony is a core concept in model selection. The idea of parsimony is to keep your models as
# simple as possible (but no simpler). This builds on the idea of Occam’s razor¹⁰⁰, in that all else being
# equal simpler explanations are better than complex ones. Simpler models are easier to interpret and
# are less finicky. Complex models often have issues with fitting and, especially, overfitting. 
# (To see a counterargument, consider Andrew Gelman’s blog.¹⁰¹.)

# Another principle that I find useful for looking at statistical models is to consider them as lenses
# through which to look at your data. (I attribute this quote to the great statistician Scott Zeger.) Under
# this philosophy, what’s the right model - whatever one connects the data to a true, parsimonious
# statement about what you’re studying.


#*******General rules for model selection*******
# Here we state a couple of general rules regarding model selection for our known knowns.

# • Omitting variables results in bias in the coefficients of interest - unless the regressors are
# uncorrelated with the omitted ones.

# I want to reiterate this point: if the omitted variable is uncorrelated with the included variables, its
# omission has no impact on estimation. It might explain some residual variation, thus it could have
# an impact on inference. As previously mentioned, this lack of impact of uncorrelated variables is
# why we randomize treatments; randomization attempts to disassociate our treatment indicator with
# variables that we don’t have to put in the model. Formal theories of inference can be designed around
# the use of randomization. However, in a practical sense, if there’s too many unobserved confounding
# variables, even randomization won’t help you, since with high probability one will stay correlated
# with the treatment.
# In most cases we won’t have randomization. So, to avoid bias, why don’t we throw everything into
# the regression model? The following rule prevents us from doing that:

#   • Including variables that we shouldn’t have increases standard errors of the regression variables.

# Actually, including any new variables increases the actual (not estimated) standard errors of other
# regressors. So we don’t want to idly throw variables into the model. In addition the model must tend
# toward perfect fit as the number of non-redundant regressors approaches the sample size. Our R2
# increases monotonically as more regressors are included, even unrelated white noise.

#R squared goes up as you put regressors in the model
n <- 100
plot(c(1, n), 0 : 1, type = "n", frame = FALSE, xlab = "p", ylab = "R^2")
y <- rnorm(n); x <- NULL; r <- NULL
for (i in 1 : n){
  x <- cbind(x, rnorm(n))
  r <- c(r, summary(lm(y ~ x))$r.squared)
}
lines(1 : n, r, lwd = 3)
abline(h = 1)