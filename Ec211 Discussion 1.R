###########################
### Ec 211: Review of R ###
###########################

# Note : In this code, we would review various
# R functions from the perspective of OLS

### Load the Libraries ###

library(MASS)
library(stargazer)
library(ggplot2)
library(reshape)


### OLS Review ###

## Simulate Data ##

set.seed(10) # make the resutls replicable

x1 = rnorm(1000, 10, 2) # simulate 1000 normal r.v. with mean = 10 and s.d. = 2
x2 = rnorm(1000, 5, 6)

cor_matrix = matrix(c(2, 1, 1, 4), nrow = 2) # correlation matrix for x3 and x4
tmp1 = mvrnorm(n = 1000, mu = c(2, 5), Sigma = cor_matrix) # simulate multivariate normal

x3 = tmp1[, 1] # x3 equals to first column of tmp1
x4 = tmp1[, 2] # x4 equals to second column of tmp1
u = rnorm(1000, 0, 3) # error term

b = c(2, -3, 3, -2, 5) # vector of beta

y = b[1] + b[2]*x1 + b[3]*x2 + b[4]*x3 + b[5]*x4 + u 


## Method 1: lm() function ##

SampleData = data.frame(y = y, x1 = x1, x2 = x2, x3 = x3, x4 = x4) # create a d.f. of the variables

method1 = lm(y ~ x1 + x2 + x3 + x4, data = SampleData) # fit the OLS model

summary(method1) # summarize the fit
names(method1) # list the information that method1 contains
print(method1$coefficients) # print the coefficients


## Method 2: Matrix Operations ##

X = cbind(1, x1, x2, x3, x4) # create matrix of independent variables with constant
Y = as.matrix(y) # matrix of dependent variable

method2_beta = solve(t(X) %*% X) %*% t(X) %*% Y # use the formula (X'X)^-1(X'Y)
print(method2_beta)
summary(method1) # compare with lm() function

method2_error = Y - X %*% method2_beta
method2_sd = sqrt(diag(solve(t(X) %*% X) *
                         sum(method2_error^2)) / (1000 - 5)) # s.e. of coefficients
print(method2_sd)
summary(method1) # compare with lm() function

method2_tvalue = method2_beta/method2_sd # t-value
print(method2_tvalue)
summary(method1)

method2_pvalue = pt(abs(method2_tvalue), df = 995, lower.tail = FALSE) * 2
print(method2_pvalue)
summary(method1)


## Method 3: Minimize SSR ##

SSR = function(betta, Y, X){
  # Function of sum of squares residual. Takes in values
  # of Y, X and betta and calculate SSR
  
  SSR = mean((Y - X %*% betta) ^ 2)
  
  return(SSR)
}

method3 = optim(par = rep(1,5), fn = SSR, Y = Y, X = X) # values are close
print(method3$par)
summary(method1)

print(method3$value) # SSR for method 3
print(mean(method1$residuals^2)) # SSR from lm()


## Latex Ouput ##

stargazer(method1) # provide latex output of table

tmp_lm1 = lm(y ~ x1 , data = SampleData) # model subset
tmp_lm2 = lm(y ~ x1 + x2, data = SampleData) # model subset
tmp_lm3 = lm(y ~ x1 + x2 + x3, data = SampleData) # model subset

stargazer(method1, tmp_lm1, tmp_lm2, tmp_lm3) # multiple results


## Plotting ##

x3_seq = seq(round(min(x3)), round(max(x3)), by = 0.01) # sequence of x3

predict_true =  b[1] + b[4]*x3_seq # true model
predict_method1 = method1$coefficients[1] + method1$coefficients[4]*x3_seq # predicted model

predictData = data.frame(True = predict_true, 
                         Predicted = predict_method1, x3 = x3_seq) # d.f. of the predicted values
View(predictData) # view the data frame
str(predictData)

predictData = melt(predictData, id = "x3") # reshape the data for ggplot
View(predictData) # view the data frame
str(predictData)

colnames(predictData) = c("x3", "Model", "PredictedValue") # rename the variables

ggplot(data = predictData, aes(x = x3, y = PredictedValue, color = Model)) +
    geom_line() + ggtitle("True Model vs Predicted Model") + scale_colour_brewer(palette="Set1") 

ggplot(data = predictData, aes(x = x3, y = PredictedValue, color = Model)) +
    geom_line() + ggtitle("True Model vs Predicted Model") 


