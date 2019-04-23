#*******Example 2.1.1*******

y = mtcars$mpg
x = cbind(1, mtcars$hp, mtcars$wt)
solve(t(x) %*% x) %*% t(x) %*% y
#[,1]
#[1,] 37.22727012
#[2,] -0.03177295
#[3,] -3.87783074
# Compare with the estimate obtained via lm
coef(lm(mpg ~ hp + wt, data = mtcars))
#(Intercept) hp wt
#37.22727012 -0.03177295 -3.87783074