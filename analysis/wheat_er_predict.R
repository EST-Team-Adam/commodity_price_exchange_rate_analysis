########################################################################
## This script examines the predictability of the exchange rates on
## the wheat price index
########################################################################

load("final.Rdata")
library(glmnet)

## Build train and test set for prediction and validation
n = NROW(final.df)
train_pct = 0.9
cut_point = round(n * train_pct)
train.df = final.df[1:cut_point, ]
test.df = final.df[(cut_point + 1):n, ]
full_time = final.df$date
train_time = train.df$date
test_time = test.df$date

## Fitting LASSO
lasso.fit = cv.glmnet(x = as.matrix(train.df[, 3:NCOL(final.df)]),
                      y = as.matrix(train.df[, 2]),
                      type.measure = "mse", nfolds = 10,
                      alpha = 0)
lasso.pred = predict(lasso.fit, newx = as.matrix(test.df[, 3:NCOL(final.df)]),
                     s = lasso.fit$lambda.1se)
lasso.fitted = predict(lasso.fit, newx = as.matrix(train.df[, 3:NCOL(final.df)]))

## Plot the fit and the prediction
## par(mfrow = c(4, 1))
## plot(full_time, final.df$log_wheat_index_igc, type = "l", ylim = c(0, 10))
## lines(train_time, lasso.fitted, col = "green", lty = 2)
## lines(test_time, lasso.pred,  col = "red", lty = 2)
plot(full_time, exp(final.df$log_wheat_index_igc), type = "l", ylim = c(0, 450),
     xlab = "Time", ylab = "Logged Wheat Index")
lines(train_time, exp(lasso.fitted), col = "green", lty = 2)
lines(test_time, exp(lasso.pred),  col = "red", lty = 2)
for(snew in seq(0, 0.1, length.out = 5)){
    lines(test_time,
          exp(predict(lasso.fit,
                      newx = as.matrix(test.df[, 3:NCOL(final.df)]),
                      s = snew)),
          col = "steelblue", lty = 2)
}


## Examine the CV error
plot(lasso.fit)

## Examine the Lasso Coef
plot(lasso.fit$glmnet.fit, "lambda", label = TRUE)
abline(v = log(lasso.fit$lambda.min), col = "red", lty = 2)
abline(v = log(lasso.fit$lambda.1se), col = "steelblue", lty = 2)

## Find the top 5 coefficients
lasso.coef = coef(lasso.fit)
nonInterceptCoef = lasso.coef[-1]
names(nonInterceptCoef) = rownames(lasso.coef)[-1]
(topCoef = head(sort(abs(nonInterceptCoef), decreasing = TRUE), 50))

## NOTE (Michael): From the fact that the most significant
##                 coefficients are always approaching the maximum lag
##                 given, it would suggest that the lag of the model
##                 is extremely large.


## NOTE (Michael): It also seems like that thet most significant lag
##                 also changes, that means the response time of
##                 trader changes over time.


dolm <- function(data){
    fit = cv.glmnet(x = as.matrix(cbind(Intercept = 1, data[, -c(1, 2)])),
                    y = as.matrix(data[, 2]))
    as.numeric(coef(fit))
}


final.zoo = as.zoo(as.matrix(final.df[1:270, -1]), final.df[1:270, 1])
final.rreg = rollapply(final.zoo, width = 260, FUN = dolm, by.column = FALSE)
