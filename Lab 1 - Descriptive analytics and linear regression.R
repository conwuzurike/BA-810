install.packages(c("data.table", "ggplot2", "ggthemes", "scales"))

library(data.table)
library(ggplot2)
library(ggthemes)
library(scales)
theme_set(theme_bw())

dt <- fread("C:/Users/chieb/Documents/Coding/School_Work/BA 810/cogo-train.tsv")

View(dt)
str(dt)

# What fraction of users use each browser?
p <- dt[,.(
  p_chrome = mean(browser1),
  p_firefox = mean(browser2),
  p_safari = mean(browser3)
)]

# Let's rearrange the dataset so that each row corresponds to a different browser. pivot_longer converts a
# "wide" dataset to a "long" dataset, where every column becomes a row.

cogo_browsers <- dt[,.(
  p_chrome= sum(browser1)/.N,
  p_firefox = sum(browser2)/.N,
  p_safari = sum(browser3)/.N
)]

cogo_browsers <- melt.data.table(cogo_browsers,
                                 measure.vars = colnames(cogo_browsers),
                                 variable.name = "browser",
                                 value.name = "p")

cogo_browsers

# plot cogo_browsers with ggplot2
ggplot(cogo_browsers, aes(browser, p)) +
  geom_bar(stat="identity") +
  scale_x_discrete("Browser") +
  scale_y_continuous("% Cogo Users", label=percent_format())

# What fraction of users use each device?
cogo_devices <- dt[, .(
  p_android = sum(device_type1) / .N,
  p_apple = sum(device_type2) / .N,
  p_other_mobile = sum(device_type3) / .N,
  p_desktop = sum(device_type4) / .N
)]

cogo_devices <- melt.data.table(
  cogo_devices,
  measure.vars = colnames(cogo_devices),
  variable.name = "device",
  value.name="p")

# plot cogo_devices with ggplot2
ggplot(cogo_devices, aes(device, p)) +
  geom_bar(stat="identity") +
  scale_x_discrete("Device") +
  scale_y_continuous("% Cogo Users", label=percent_format())

# What fraction of users use more than one browser?

# create a column that adds hot coded browsers
dt[, num_browsers:=browser1 + browser2 + browser3]
# Boolean if greater than 1
dt[, at_least_one_browser:=num_browsers > 1]

# takethe average by filtering with at_least_one_browser
mean(dt$at_least_one_browser)

# What is the distribution of email open-rates?
ggplot(dt, aes(p_open)) +
  geom_histogram(bins=30) +
  stat_bin(binwidth = .2)

#  plotted a cumulative distribution
ggplot(dt, aes(p_open)) +
  scale_y_continuous("Pr[p_open < Y]") +
  scale_x_continuous("Open rate") +
  stat_ecdf()

#Some extra questions you may want to consider:
# 1. Are multi-homing users (those using more than one device) more or less likely to open an email?
# 2. On which device are multi-homing users most likely to open an email?
# 3. What is the relationship between the various activity indicators and open rates?
# 4. Using faceting, plot the relationship between age and p_open by gender and/or state.

dt[, ran1 := runif(.N, min = 0, max = 100)]
dt[, tensdig := as.integer(user_id / 10) %% 10]

set.seed(810)
rnorm(1)
# will assign 100k random rows to the test set
test_index <- sample(nrow(dt), 100000)

# split test vs train
dt.test <- dt[test_index,]
dt.train <- dt[-test_index,]

# here's one simple formula -- it's up to your to add more predictors as you see fit (haha)
f1 <- as.formula(p_open ~ browser1 + browser2 + browser3)

y.train <- dt.train$p_open
y.test <- dt.test$p_open

fit.lm1 <- lm(f1, dt.train)

yhat.train.lm1 <- predict(fit.lm1)
mse.train.lm1 <- mean((y.train - yhat.train.lm1)^2)
mse.train.lm1

yhat.test.lm1 <- predict(fit.lm1, dt.test)
mse.test.lm1 <- mean((y.test - yhat.test.lm1)^2)
mse.test.lm1

# another fomrula to try
f2 <- as.formula(p_open ~ browser1 + browser2 + browser3 + age + gender)
fit.lm2 <- lm(f2, dt.train)

yhat.train.lm2 <- predict(fit.lm2)
mse.train.lm2 <- mean((y.train - yhat.train.lm2)^2)
mse.train.lm2

yhat.test.lm2 <- predict(fit.lm2, dt.test)
mse.test.lm2 <- mean((y.test - yhat.test.lm2)^2)
mse.test.lm2

# what if we had a smaller sample like 30 instead of 188298 observations
dt.train.small.sample.size <- 30
dt.train.small.sample <- dt.train[sample(nrow(dt.train), dt.train.small.sample.size), ]
y.train.small.sample <- d.train.small.sample$p_open

# another fomrula to try
f6 <- as.formula(p_open ~ activity_locations + age + age*activity_locations)
fit.lm6 <- lm(f6, dt.train)
coef(fit.lm6)

yhat.train.lm6 <- predict(fit.lm6)
mse.train.lm6 <- mean((y.train - yhat.train.lm6)^2)
mse.train.lm6

yhat.test.lm6 <- predict(fit.lm6, dt.test)
mse.test.lm6 <- mean((y.test - yhat.test.lm6)^2)
mse.test.lm6

# Lets experiment with a subset of the data. First without randomness then with
# subset of age greater than 26 not randomized
dt.train.nr <- dt.train[dt.train$age>26,]
y.train.nr <- dt.train.nr$p_open
dt.test.nr <- dt.test[dt.test$age>26,]
y.test.nr <- dt.test.nr$p_open

fit.lm2.nr <- lm(f2, dt.train.nr)

yhat.train.lm2.nr <- predict(fit.lm2.nr)
mse.train.lm2.nr <- mean((y.train.nr - yhat.train.lm2.nr)^2)
mse.train.lm2.nr

yhat.test.lm2.nr <- predict(fit.lm2.nr, dt.test.nr)
mse.test.lm2.nr <- mean((y.test.nr - yhat.test.lm2.nr)^2)
mse.test.lm2.nr

# subset of age greater than 26 randomized
dt.train.r <- dt.train[sample(nrow(dt.train), nrow(dt.train.nr)),]#
y.train.r <- dt.train.r$p_open
dt.test.r <- dt.test[sample(nrow(dt.test), nrow(dt.test.nr)),]
y.test.r <- dt.test.r$p_open

fit.lm2.r <- lm(f2, dt.train.r)

yhat.train.lm2.r <- predict(fit.lm2.r)
mse.train.lm2.r <- mean((y.train.r - yhat.train.lm2.r)^2)
mse.train.lm2.r

yhat.test.lm2.r <- predict(fit.lm2.r, dt.test.r)
mse.test.lm2.r <- mean((y.test.r - yhat.test.lm2.r)^2)
mse.test.lm2.r

# MSE was lower for data set that had randomness even if it was a subset of data


