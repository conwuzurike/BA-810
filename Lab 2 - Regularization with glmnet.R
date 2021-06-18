library(data.table)
library(ggplot2)
library(ggthemes)
library(scales)
library(glmnet)
theme_set(theme_bw())

dd <- fread("C:/Users/chieb/Documents/Coding/School_Work/BA 810/cogo-train.tsv", stringsAsFactors = T)
# stringsAsFactor - Convert all character columns to factors?

# Start by splitting your training data into a test data set. 
dd[, test:=0] # Creating New column to identify test
dd[sample(nrow(dd), 100000), test:=1] # take 100K random rows and stick them in the test set
# now split
dd.test <- dd[test==1]
dd.train <- dd[test==0]

# Data contains 188298 observation. Lets randomly subset the data and so it much more workable
dd.train.sample.size <- 15000
dd.train.sample <- dd.train[sample(nrow(dd.train), dd.train.sample.size)]
dd.test.sample <- dd.test[sample(nrow(dd.test), dd.train.sample.size)]

# Formula 1
f1 <- as.formula(p_open ~ browser1 + browser2 + browser3)

# the [, -1] means take all columns of the matrix except the first column, which is an intercept added by default
x1.train.sample <- model.matrix(f1, dd.train.sample)[, -1]
x1.test.sample <- model.matrix(f1, dd.test.sample)[, -1]

# and this the response
y.train <- dd.train$p_open
y.train.sample <- dd.train.sample$p_open
y.test <-dd.test$p_open
y.test.sample <-dd.test.sample$p_open

# Simple Linear
fit.lm <- lm(f1, dd.train.sample)

# MSE on the training data.
yhat.train.lm <- predict(fit.lm)
mse.train.lm <- mean((y.train.sample - yhat.train.lm)^2)
mse.train.lm

yhat.test.lm <- predict(fit.lm, dd.test.sample)
mse.test.lm <- mean((y.test.sample - yhat.test.lm)^2)
mse.test.lm

# Ridge Regression time
# Ridge shrinks coefficients towards zero.Therefore,by setting alpha = 0, we invoke ridge regression
fit.ridge <- glmnet(x1.train.sample, y.train.sample, alpha = 0)
fit.ridge$lambda

# examine the coefficients associated with a particular ??
predict(fit.ridge,
        type = "coefficients",
        s = fit.ridge$lambda[1]) # [1] indexs first ??  which is the largest

predict(fit.ridge,
        type = "coefficients",
        s = fit.ridge$lambda[10]) # indexes the 10th vaule for lamnda ??

#Notice the ??1 > ??10 hence the coefficients we get for the 10th value of ?? are bigger
# ?? increasing lamnda decreases coefficients

#Matrix of coefficients
ridge.coef <- predict(fit.ridge,
                      type = "coefficients",
                      s = fit.ridge$lambda)
dim(ridge.coef) # 4 Rows and 98 columns

# plot the coefficient associated with a predictor as a function of lambda,
to_plot <- data.table(
  lambda = fit.ridge$lambda,
  coef_value = ridge.coef[2, ]
)
#Here we plotted the value of the 2nd feature against log(??)
#(it is often the case that we plot against log?? to get a prettier picture.)
ggplot(to_plot, aes(log(lambda), coef_value)) +
  geom_line() +
  theme_few()

# Typically, we will use the cv.glmnet command to automatically select the best value for the ?? hyper-parameter.
# glmnet automatically tries multiple ??'s and selects the best one using cross-validation. 
# The nfolds parameter tells glmnet to do 10-fold cross-validation.
fit.ridge <- cv.glmnet(x1.train.sample, y.train.sample, alpha = 0, nfolds = 10)

# Compute MSE Train. The s parameter to predict is essentially ??
yhat.train.ridge <- predict(fit.ridge, x1.train.sample, s = fit.ridge$lambda.min)
mse.train.ridge <- mean((y.train.sample - yhat.train.ridge)^2)
mse.train.ridge

# Compute MSE Test The s parameter to predict is essentially ??
yhat.test.ridge <- predict(fit.ridge, x1.test.sample, s = fit.ridge$lambda.min)
mse.test.ridge <- mean((y.test.sample - yhat.test.ridge)^2)
mse.test.ridge

# Lets try using Lasso instead
# glmnet can also do lasso regression, and it's as simple as setting alpha = 1
fit.lasso <- cv.glmnet(x1.train.sample, y.train.sample, alpha = 1, nfolds = 10)


yhat.train.lasso <- predict(fit.lasso, x1.train.sample, s = fit.lasso$lambda.min)
mse.train.lasso <- mean((y.train.sample - yhat.train.lasso)^2)
mse.train.lasso

yhat.test.lasso <- predict(fit.lasso, x1.test.sample, s = fit.lasso$lambda.min)
mse.test.lasso <- mean((y.test.sample - yhat.test.lasso)^2)
mse.test.lasso

