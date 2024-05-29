#
# 1. IMPORTING LIBRARIES.
#

library(tidyverse)
library(readxl)
library(ISLR2)
library(MASS)
library(leaps)
library(glmnet)
library(pls)
library(splines)
library(e1071)

# Setting random seed at 42.
set.seed(42)

#
# 2. DATA HANDLING
#

# 2.1. Reading Health data set into table.
df1 <- read_excel("Health.xlsx")

# 2.2.1. Reading Gas Emission Data set into tables.
df2_1 <- read.csv("./pp_gas_emission/gt_2011.csv")
df2_1$year <- 1

df2_2 <- read.csv("./pp_gas_emission/gt_2012.csv")
df2_2$year <- 2

df2_3 <- read.csv("./pp_gas_emission/gt_2013.csv")
df2_3$year <- 3

df2_4 <- read.csv("./pp_gas_emission/gt_2014.csv")
df2_4$year <- 4

df2_5 <- read.csv("./pp_gas_emission/gt_2015.csv")
df2_5$year <- 5

# 2.2.2. Concatenating partitions of tables of loaded data.
df2 <- rbind(df2_1, df2_2, df2_3, df2_4, df2_5) 

# 2.2.3. Removing temporary tables.
rm(df2_1, df2_2, df2_3, df2_4, df2_5)

# 2.3. Making data frames out of tables of loaded data.

df1 <- data.frame(df1)
df2 <- data.frame(df2)

# 2.4. Attaching Data Frames.

attach(df1)
attach(df2)

# 2.5. Partitioning data sets.
sample1 <- sample(c(TRUE, FALSE), nrow(df1), replace=TRUE, prob=c(0.8,0.2))
sample2 <- sample(c(TRUE, FALSE), nrow(df2), replace=TRUE, prob=c(0.8,0.2))

train1 <- df1[sample1,]
test1 <- df1[!sample1,]
train2 <- df2[sample2,]
test2 <- df2[!sample2,]
rm(sample1, sample2)

xtrain1 <- model.matrix(X1 ~ ., train1)[,-1]
ytrain1 <- train1$X1
xtest1 <- model.matrix(X1 ~ ., test1)[,-1]
ytest1 <- test1$X1

xtrain2 <- model.matrix(NOX ~ ., train2)[,-1]
ytrain2 <- train2$NOX
xtest2 <- model.matrix(NOX ~ ., test2)[,-1]
ytest2 <- test2$NOX


#
# 3. MULTIPLE LINEAR REGRESSION (MLR)
# 

# 3.1. MLR on Health data set.

# 3.1.1. Fitting Model.
lm1.fit <- lm(formula = X1 ~ X2 + X3 + X4 + X5,
              data = df1,
              method = "qr"
              )
print(summary(lm1.fit))

# Test MSE - Linear Regression
lm1.pred <- predict(lm1.fit, test1)
lm1.mse <- mean((ytest1 - lm1.pred)^2)
print(lm1.mse)

# 3.1.2. Subset Selection.

# 3.1.2.1. Best Subset Selection (BSS)
lm1_bss <- regsubsets(X1 ~ ., train1,
                      method = "exhaustive")
print(summary(lm1_bss))
print(summary(lm1_bss)$cp)
print(summary(lm1_bss)$bic)
print(summary(lm1_bss)$adjr2)

# 3.1.2.2. Forward Stepwise Selection (FwSS)
lm1_fwss <- regsubsets(X1 ~ ., train1,
                       method = "forward")
print(summary(lm1_fwss))
print(summary(lm1_fwss)$cp)
print(summary(lm1_fwss)$bic)
print(summary(lm1_fwss)$adjr2)

# 3.1.2.3. Backward Stepwise Selection (BwSS)
lm1_bwss <- regsubsets(X1 ~ ., train1,
                       method = "backward")
print(summary(lm1_bwss))
print(summary(lm1_bwss)$cp)
print(summary(lm1_bwss)$bic)
print(summary(lm1_bwss)$adjr2)

#
# According to Subset Selection, the best model would be just with X5 as the single variable.
#

# Test MSE - After Subset Selection
lm1_ss.fit <- lm(formula = X1 ~ X5,
                 data = df1,
                 method = "qr"
                 )
lm1_ss.pred <- predict(lm1_ss.fit, test1)
lm1_ss.mse <- mean((ytest1 - lm1_ss.pred)^2)
print(lm1_ss.mse)


# 3.1.3. Lasso
lm1_lasso_out <- cv.glmnet(y = ytrain1, 
                           x = xtrain1,
                           alpha=1)
print(lm1_lasso_out$lambda.min)
lm1_lasso.coef <- predict(lm1_lasso_out, s=lm1_lasso_out$lambda.min, newx = xtest1, type="coefficients")
print(lm1_lasso.coef)

# Test MSE - Lasso
lm1_lasso.pred <- predict(lm1_lasso_out, s=lm1_lasso_out$lambda.min, newx = xtest1)
lm1_lasso.mse <- mean((lm1_lasso.pred - ytest1)^2)

# 3.1.4. Ridge
lm1_ridge_out <- cv.glmnet(y = ytrain1, 
                           x = xtrain1,
                           alpha=0)
print(lm1_ridge_out$lambda.min)
lm1_ridge.coef <- predict(lm1_ridge_out, s=lm1_ridge_out$lambda.min, newx = xtest1, type="coefficients")
print(lm1_ridge.coef)

# Test MSE - Ridge
lm1_ridge.pred <- predict(lm1_ridge_out, s=lm1_ridge_out$lambda.min, newx = xtest1)
lm1_ridge.mse <- mean((lm1_ridge.pred - ytest1)^2)


# 3.2. MLR on Gas Emission Data Set.

# 3.2.1. Fitting Model.
lm21.fit <- lm(formula = NOX ~ AT + AP + AH + AFDP + GTEP + TIT + TAT + TEY + CDP + CO + year,
              data = df2,
              method = "qr"
)
print(summary(lm21.fit))

# 
# Upon closely examining the the p-values of coeffiecients of all the variables, it can be
# noted that the "AFDP", "GTEP", and "CDP" have higher values in comparison to other variables.
# We can attempt a second MLR model for comparison.
# 

# 3.2.2. Refitting Model.
lm22.fit <- lm(formula = NOX ~ AT + AP + AH + TIT + TAT + TEY + CO + year,
               data = df2,
               method = "qr"
)
print(summary(lm22.fit))

#
#
# From the training R2 scores we can use either models as the results are almost similar.
# We've used all the variables in this section.
#
#

# Fixing initial model.
lm2.fit <- lm21.fit
rm(lm21.fit, lm22.fit)

# Test MSE - Linear Regression
lm2.pred <- predict(lm2.fit, test2)
lm2.mse <- mean((ytest2 - lm2.pred)^2)
print(lm2.mse)


# 3.2.3. Subset Selection.

# 3.2.2.1. Best Subset Selection (BSS)
lm2_bss <- regsubsets(NOX ~ ., train2, nvmax = 12,
                      method = "exhaustive")
print(summary(lm2_bss))
print(summary(lm2_bss)$cp)
print(summary(lm2_bss)$bic)
print(summary(lm2_bss)$adjr2)

# 3.2.2.2. Forward Subset Selection (FwSS)
lm2_fwss <- regsubsets(NOX ~ ., train2, nvmax = 12,
                       method = "forward")
print(summary(lm2_fwss))
print(summary(lm2_fwss)$cp)
print(summary(lm2_fwss)$bic)
print(summary(lm2_fwss)$adjr2)

# 3.2.2.3. Backward Subset Selection (BwSS)
lm2_bwss <- regsubsets(NOX ~ ., train2,nvmax = 12,
                       method = "backward")
print(summary(lm2_bwss))
print(summary(lm2_bwss)$cp)
print(summary(lm2_bwss)$bic)
print(summary(lm2_bwss)$adjr2)


#
# According to Subset Selection, the best model would be the one with 8 variable including:
# AT, AP, AH, TIT, TAT, TEY, CO, year. Curiously, this coincides with the initial p-value 
# hypothesis of not including AFDP, GTEP, and CDP in MLR.
#

# Test MSE - Subset Selection
lm2_ss.fit <- lm(formula = NOX ~ AT + AP + AH + TIT + TAT + TEY + CO + year,
                 data = df2,
                 method = 'qr'
                 )
lm2_ss.pred <- predict(lm2_ss.fit, test2)
lm2_ss.mse <- mean((ytest2 - lm2.pred)^2)
print(lm2_ss.mse)


# 3.2.3. Lasso
lm2_lasso_out <- cv.glmnet(y = ytrain2, 
                           x = xtrain2,
                           alpha=1)
print(lm2_lasso_out$lambda.min)
lm2_lasso.coef <- predict(lm2_lasso_out, s=lm2_lasso_out$lambda.min, newx = xtest2, type="coefficients")
print(lm2_lasso.coef)

# Test MSE - Lasso
lm2_lasso.pred <- predict(lm2_lasso_out, s=lm2_lasso_out$lambda.min, newx = xtest2)
lm2_lasso.mse <- mean((lm2_lasso.pred - ytest2)^2)


# 3.2.4. Ridge
lm2_ridge_out <- cv.glmnet(y = ytrain2, 
                           x = xtrain2,
                           alpha=0)
print(lm2_ridge_out$lambda.min)
lm2_ridge.coef <- predict(lm2_ridge_out, s=lm2_ridge_out$lambda.min, newx = xtest2, type="coefficients")
print(lm2_ridge.coef)

# Test MSE - Ridge
lm2_ridge.pred <- predict(lm2_ridge_out, s=lm2_ridge_out$lambda.min, newx = xtest2)
lm2_ridge.mse <- mean((lm2_ridge.pred - ytest2)^2)


#
# 4. POLYNOMIAL REGRESSION (PR)
#

# 4.1. PR on Health data set.

# 4.1.1. Fitting Model.
pm1.fit <- lm(formula = X1 ~ poly(X2,3) + poly(X3, 3) + poly(X4, 3) + poly(X5, 3),
              data = df1,
              method = "qr"
)
print(summary(pm1.fit))

# Test MSE - Polynomial Regression
pm1.pred <- predict(pm1.fit, test1)
pm1.mse <- mean((ytest1 - pm1.pred)^2)
print(pm1.mse)

# 4.1.2. Lasso
pm1_lasso_out <- cv.glmnet(y = ytrain1,
                           x = xtrain1,
                           alpha = 1)
print(pm1_lasso_out$lambda.min)
pm1_lasso.coef <- predict(pm1_lasso_out, s=pm1_lasso_out$lambda.min, newx = xtest1, type="coefficients")
print(pm1_lasso.coef)

# Test MSE - Lasso
pm1_lasso.pred <- predict(pm1_lasso_out, s=pm1_lasso_out$lambda.min, newx = xtest1)
pm1_lasso.mse <- mean((pm1_lasso.pred - ytest1)^2)

# 4.1.3. Ridge
pm1_ridge_out <- cv.glmnet(y = ytrain1, 
                           x = xtrain1,
                           alpha=0)
print(pm1_ridge_out$lambda.min)
pm1_ridge.coef <- predict(pm1_ridge_out, s=pm1_ridge_out$lambda.min, newx = xtest1, type="coefficients")
print(pm1_ridge.coef)

# Test MSE  - Ridge
pm1_ridge.pred <- predict(pm1_ridge_out, s=pm1_ridge_out$lambda.min, newx = xtest1)
pm1_ridge.mse <- mean((pm1_ridge.pred - ytest1)^2)


# 4.2. MLR on Gas Emission Data Set.

# 4.2.1. Fitting Model.
pm2.fit <- lm(formula = NOX ~ poly(AT,3) + poly(AP,3) + poly(AH,3) + poly(AFDP,3) + poly(GTEP,3) + 
                 poly(TIT,3) + poly(TAT,3) + poly(TEY,3) + poly(CDP,3) + poly(CO,3) + poly(year,3),
              data = df2,
              method = "qr"
)
print(summary(pm2.fit))

# Test MSE - Polynomial Regression
pm2.pred <- predict(pm2.fit, test1)
pm2.mse <- mean((ytest1 - pm2.pred)^2)
print(pm2.mse)


# 4.2.2. Lasso
pm2_lasso_out <- cv.glmnet(y = ytrain2, 
                           x = xtrain2,
                           alpha=1)
print(pm2_lasso_out$lambda.min)
pm2_lasso.coef <- predict(pm2_lasso_out, s=pm2_lasso_out$lambda.min, newx = xtest2, type="coefficients")
print(pm2_lasso.coef)

# Test MSE - Lasso
pm2_lasso.pred <- predict(pm2_lasso_out, s=pm2_lasso_out$lambda.min, newx = xtest2)
pm2_lasso.mse <- mean((pm2_lasso.pred- ytest2)^2)

# 4.2.3. Ridge
pm2_ridge_out <- cv.glmnet(y = ytrain2, 
                           x = xtrain2,
                           alpha=0)
print(pm2_ridge_out$lambda.min)
pm2_ridge.coef <- predict(pm2_ridge_out, s=pm2_ridge_out$lambda.min, newx = xtest2, type="coefficients")
print(pm2_ridge.coef)

# Test MSE - Ridge
pm2_ridge.pred <- predict(pm2_ridge_out, s=pm2_ridge_out$lambda.min, newx = xtest2)
pm2_ridge.mse <- mean((pm2_ridge.pred - ytest2)^2)


#
# 5. PRINCIPAL COMPONENTS REGRESSION (PCR)
#

# 5.1. PCR on Health Data

pcr_model1.fit <- pcr(formula = X1 ~ ., 
                 data = train1, 
                 scale = TRUE,
                 validation = "CV"
                 )
print(summary(pcr_model1.fit))

# Validation plot to determine the number of components to be used as M.
validationplot(pcr_model1.fit , val.type = "MSEP")

# Test MSE - M = 3.
pcr_model1.pred <- predict(pcr_model1.fit, xtest1, ncomp = 3)
pcr_model1.mse <- mean((pcr_model1.pred - ytest1)^2)
print(pcr_model1.mse)

# 5.2. PCR on Gas Emission Data Set

pcr_model2.fit <- pcr(formula = NOX ~ ., 
                     data = train2, 
                     scale = TRUE,
                     validation = "CV"
)
print(summary(pcr_model2.fit))

# Validation plot to determine the number of components to be used as M.
validationplot(pcr_model2.fit , val.type = "MSEP")

# Test MSE - M = 8.
pcr_model2.pred <- predict(pcr_model2.fit, xtest2, ncomp = 8)
pcr_model2.mse <- mean((pcr_model2.pred - ytest2)^2)
print(pcr_model2.mse)


#
# 6. PARTIAL LEAST SQUARES REGRESSION (PLSR)
#

# 6.1. PLSR on Health Data

plsr_model1.fit <- plsr(formula = X1 ~ ., 
                      data = train1, 
                      scale = TRUE,
                      validation = "CV"
)
print(summary(plsr_model1.fit))

# Validation plot to determine the number of components to be used as M.
validationplot(plsr_model1.fit , val.type = "MSEP")

# Test MSE - M = 2.
plsr_model1.pred <- predict(plsr_model1.fit, xtest1, ncomp = 2)
plsr_model1.mse <- mean((plsr_model1.pred - ytest1)^2)
print(plsr_model1.mse)

# 6.2. PLSR on Gas Emission Data Set

plsr_model2.fit <- plsr(formula = NOX ~ ., 
                      data = train2, 
                      scale = TRUE,
                      validation = "CV"
)
print(summary(plsr_model2.fit))

# Validation plot to determine the number of components to be used as M.
validationplot(plsr_model2.fit , val.type = "MSEP")

# Test MSE - M = 9.
plsr_model2.pred <- predict(plsr_model2.fit, xtest2, ncomp = 9)
plsr_model2.mse <- mean((plsr_model2.pred - ytest2)^2)
print(plsr_model2.mse)


#
# 7. CUBIC SPLINES (BS)
#


# 7.1. BS on Health Data
 
# Using cross-validation to determine degrees of freedom.
bs1.cvmse <- c()
i <- 1
for (p in 3:6) {
  model_temp <- lm(X1 ~ bs(X2, df = p) + bs(X3, df = p) + bs(X4, df = p) + bs(X5, df = p), 
                   data = train1)
  
  bs1.cvmse[i] <- mean(model_temp$residuals^2)
  i <- i+1
  rm(model_temp)
}
p1 <- which.min(bs1.cvmse) + 2

# Model Fitting.
bs1_model.fit <- lm(X1 ~ bs(X2, df = p1) + bs(X3, df = p1) + bs(X4, df = p1) + bs(X5, df = p1),
                    data = train1)

# Test MSE.
bs1_model.pred <- predict(bs1_model.fit,  test1)
bs1_model.mse <- mean((ytest1 - bs1_model.pred)^2)
print(bs1_model.mse)

# 7.2. BS on Gas Emission Data Set.

# Using cross-validation to determine degrees of freedom.
bs2.cvmse <- c()
i <- 1
for (p2 in 3:50) {
  model_temp <- lm(NOX ~ bs(AT, df = p2) + bs(AP, df = p2) + bs(AH, df = p2) + bs(AFDP, df = p2) +
                     bs(GTEP, df = p2) + bs(TIT, df = p2) + bs(TAT, df = p2) + bs(TEY, df = p2) +
                     bs(CDP, df = p2) + bs(CO, df = p2) + bs(year, df = p2), 
                   data = train2)
  
  bs2.cvmse[i] <- mean(model_temp$residuals^2)
  i <- i+1
  rm(model_temp)
}
p2 <- which.min(bs2.cvmse) + 2

# Model Fitting.
bs2_model.fit <- lm(NOX ~ bs(AT, df = p2) + bs(AP, df = p2) + bs(AH, df = p2) + bs(AFDP, df = p2) +
                      bs(GTEP, df = p2) + bs(TIT, df = p2) + bs(TAT, df = p2) + bs(TEY, df = p2) +
                      bs(CDP, df = p2) + bs(CO, df = p2) + bs(year, df = p2),
                    data = train2)

# Test MSE.
bs2_model.pred <- predict(bs2_model.fit,  test2)
bs2_model.mse <- mean((ytest2 - bs2_model.pred)^2)
print(bs2_model.mse)

#
#
# 8. SUPPORT VECTOR MACHINE (SVM)
# 
# 

# 8.1. SVM on Health Data.

# 8.1.1. Linear Kernel
svm1_lin_out <- tune(svm, X1 ~ ., data = train1, kernel = "linear", 
                         range = list(cost = c(0.01:100)),
                         scale = TRUE
                         )
svm1_lin.fit <- svm1_lin_out$best.model
print(summary(svm1_lin.fit))

# Test MSE - Linear Kernel
svm1_lin.pred <- predict(svm1_lin.fit, xtest1)
svm1_lin.mse <- mean((ytest1 - svm1_lin.pred)^2)
print(svm1_lin.mse)

# 8.1.2. Polynomial Kernel
svm1_pol_out <- tune.svm(X1 ~ ., data = train1, kernel = "polynomial", 
                         range = list(degree = c(2:5)),
                         scale = TRUE
                         )
svm1_pol.fit <- svm1_pol_out$best.model
print(summary(svm1_pol.fit))

# Test MSE - Polynomial Kernel
svm1_pol.pred <- predict(svm1_pol.fit, xtest1)
svm1_pol.mse <- mean((ytest1 - svm1_pol.pred)^2)
print(svm1_pol.mse)

# 8.1.3. Radial Kernel
svm1_rad_out <- tune.svm(X1 ~ ., data = train1, kernel = "radial", 
                         range = list(cost = c(10^c(-3:5)), gamma = 10^c(-2:2)),
                         scale = TRUE
                         )
svm1_rad.fit <- svm1_rad_out$best.model
print(summary(svm1_rad.fit))

# Test MSE - Radial Kernel
svm1_rad.pred <- predict(svm1_rad.fit, xtest1)
svm1_rad.mse <- mean((ytest1 - svm1_rad.pred)^2)
print(svm1_rad.mse)


# 8.2. SVM on Gas Emission Data Set.

# 8.2.1. Linear Kernel
svm2_lin_out <- tune.svm(NOX ~ ., data = train2, kernel = "linear", 
                         range = list(cost = c(0.01:100)),
                         scale = TRUE
)
svm2_lin.fit <- svm2_lin_out$best.model
print(summary(svm2_lin.fit))

# Test MSE - Linear Kernel
svm2_lin.pred <- predict(svm2_lin.fit, xtest2)
svm2_lin.mse <- mean((ytest2 - svm2_lin.pred)^2)
print(svm2_lin.mse)

# 8.2.2. Polynomial Kernel
svm2_pol_out <- tune.svm(NOX ~ ., data = train2, kernel = "polynomial", 
                         range = list(degree = c(2:5)),
                         scale = TRUE
)
svm2_pol.fit <- svm2_pol_out$best.model
print(summary(svm1_pol.fit))

# Test MSE - Polynomial SVM
svm2_pol.pred <- predict(svm2_pol.fit, xtest2)
svm2_pol.mse <- mean((ytest2 - svm2_pol.pred)^2)
print(svm2_pol.mse)

# 8.2.3. Radial Kernel
svm2_rad_out <- tune.svm(NOX ~ ., data = train2, kernel = "radial", 
                         range = list(cost = c(10^c(-1:4))),
                         scale = TRUE
                         )
svm2_rad.fit <- svm2_rad_out$best.model
print(summary(svm2_rad.fit))

# Test MSE - Radial SVM
svm2_rad.pred <- predict(svm2_rad.fit, xtest2)
svm2_rad.mse <- mean((ytest2 - svm2_rad.pred)^2)
print(svm2_rad.mse)


#
# 9. BEST MODEL
#

model_names <- c("1.	Linear Regression", "2.	Linear Regression after Subset Selection", "3.	Linear Lasso",
                 "4.	Linear Ridge", "5.	Polynomial Regression", "6.	Polynomial Lasso", 
                 "7.	Polynomial Ridge", "8.  Principle Component Regression", 
                 "9.	 Partial Least Squares Regression", "10.	 Cubic Spline ", "11.  SVM Linear Kernel",
                 "12.	 SVM Polynomial Kernel", "13.	 SVM Radial Kernel")

# 9.1. Best Model for Health Data Set.
model_mses1 <- c(lm1.mse, lm1_ss.mse, lm1_lasso.mse, lm1_ridge.mse, pm1.mse, pm1_lasso.mse, pm1_ridge.mse,
                pcr_model1.mse, plsr_model1.mse, bs1_model.mse, svm1_lin.mse, svm1_pol.mse, svm1_rad.mse)

for (i in 1:13) {
  print(model_names[i])
  print(model_mses1[i])
}
b1 <- which.min(model_mses1)
print(model_names[b1])

# 9.2. Best Model for Gas Emission Data Set.
model_mses2 <- c(lm2.mse, lm2_ss.mse, lm2_lasso.mse, lm2_ridge.mse, pm2.mse, pm2_lasso.mse, pm2_ridge.mse,
                pcr_model2.mse, plsr_model2.mse, bs2_model.mse, svm2_lin.mse, svm2_pol.mse, svm2_rad.mse)

for (i in 1:13) {
  print(model_names[i])
  print(model_mses2[i])
}
b2 <- which.min(model_mses2)
print(model_names[b2])
 
 
 
 
 
 
 