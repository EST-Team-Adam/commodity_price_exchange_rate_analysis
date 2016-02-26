########################################################################
## This script examines the predictability of the exchange rates on
## the wheat price index
########################################################################

load("final.Rdata")
library(glmnet)
library(zoo)

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


## NOTE (Michael): It also seems like that thet most significant lag
##                 also changes, that means the response time of
##                 trader changes over time.


dolm <- function(data){
    fit = cv.glmnet(x = as.matrix(cbind(Intercept = 1, data[, -c(1, 2)])),
                    y = as.matrix(data[, 2]))
    as.numeric(coef(fit))
}

window.size = 260
final.zoo = as.zoo(as.matrix(final.df[, -1]), final.df[, 1])
final.rreg = rollapply(final.zoo, width = 260, FUN = dolm, by.column = FALSE)
rreg.df = as.data.frame(final.rreg)
colnames(rreg.df) = c("Intercept", colnames(final.df)[-c(1, 2)])
rownames(rreg.df) = final.df[(window.size):NROW(final.df), "date"]


notAllZero = function(x){
    !all(x == 0)
}

nonZeroCoef = colnames(rreg.df)[sapply(rreg.df, notAllZero)]
rregNonZero.df = rreg.df[, nonZeroCoef]

par(mfrow = c(2, 1), mar = c(2.1, 4.1, 4.1, 1))
plot(final.df[window.size:NROW(final.df), "date"],
     final.df[window.size:NROW(final.df), "log_wheat_index_igc"],
     type = "l", ylab = "Logged Wheat Price")
for(i in 2:NCOL(rregNonZero.df)){
    name = colnames(rregNonZero.df)[i]
    if(i == 2)
        plot(as.Date(rownames(rregNonZero.df)), rregNonZero.df[, name], type = "l",
             ylim = c(-0.1, 1))
    if(max(abs(rregNonZero.df[, name])) >= 0.079)
        lines(as.Date(rownames(rregNonZero.df)), rregNonZero.df[, name])
}


## Most significant lag
checkLag = function(x){
    nonZero = x[which(x != 0)]
    ## sort(nonZero, decreasing = TRUE)
    sort(unique(as.numeric(gsub("[^0-9]", "", names(nonZero)))))
}

par(mfrow = c(2, 1))
lagNum = apply(rregNonZero.df, 1, checkLag)
with(final.df[window.size:NROW(final.df), ],
     plot(date, log_wheat_index_igc, type = "l"))
plot(as.Date(names(lagNum)), sapply(lagNum, max), type = "l")


checkLag = function(x){
    lagNum = as.numeric(gsub("[^0-9]", "", names(x)))
    filter =  lagNum > 2 & !is.na(lagNum)
    maxLag = which.max(x[filter])
    as.numeric(gsub("[^0-9]", "", names(x)[maxLag]))
}

par(mfrow = c(2, 1))
lagNum = apply(rregNonZero.df, 1, checkLag)
with(final.df[window.size:NROW(final.df), ],
     plot(date, log_wheat_index_igc, type = "l"))
plot(as.Date(names(lagNum)), lagNum, type = "l")


par(mfrow = c(5, 1), mar = c(2.1, 4.1, 0, 1))
plot(final.df[window.size:NROW(final.df), "date"],
     final.df[window.size:NROW(final.df), "log_wheat_index_igc"],
     type = "l", ylab = "Logged Wheat Price")
plot(as.Date(rownames(rregNonZero.df)), rregNonZero.df$USD.EUR_lag1, type = "l")
plot(as.Date(rownames(rregNonZero.df)), rregNonZero.df$USD.GBP, type = "l")
plot(as.Date(rownames(rregNonZero.df)), rregNonZero.df$USD.CAD, type = "l")
plot(as.Date(rownames(rregNonZero.df)), rregNonZero.df$USD.NZD, type = "l")

## Here we extract the top 15 significant variables excluding the
## intercept to fit the relaxed regression.
nComponents = 5
sigCoefVar = names(head(sort(sapply(rregNonZero.df[-1],
                                    function(x) max(abs(x))), decreasing = TRUE),
                        nComponents))
relaxedReg.df = final.df[, c("log_wheat_index_igc", "date", sigCoefVar)]

## Run rolling relaxed regression
window.size = 260
rollingInd =
    lapply(0:(NROW(final.df) - window.size),
           FUN = function(x) x + (1:window.size))

rollRelaxReg = 
    lapply(rollingInd,
           FUN = function(ind){
               lm(log_wheat_index_igc ~. -date, data = relaxedReg.df[ind, ])
           })



## Coefficients of the relaxed rolling regression
rollRelaxRegCoef.lst = lapply(rollRelaxReg, coef)
rollRelaxRegCoef.df =
    data.frame(Reduce(f = function(x, y) rbind(x, y), rollRelaxRegCoef.lst))
colnames(rollRelaxRegCoef.df)[1] = "Intercept"

par(mfrow = c(ceiling((nComponents + 2)/2), 2), mar = rep(0, 4))
with(final.df[-c(1:window.size), ],
     plot(date, log_wheat_index_igc, type = "l"))
for(i in 1:NCOL(rollRelaxRegCoef.df)){
    plot(final.df[(window.size):NROW(final.df), "date"],
         rollRelaxRegCoef.df[, colnames(rollRelaxRegCoef.df)[i]],
         type = "l", ylim = range(rollRelaxRegCoef.df))
    legend("topleft", colnames(rollRelaxRegCoef.df)[i], bty = "n")
    abline(h = 0, col = "red", lty = 2)
}


## Show the fit components
fitComponent =
    as.matrix(final.df[window.size:NROW(final.df), sigCoefVar]) *
    as.matrix(rollRelaxRegCoef.df[, -1])
par(mfrow = c(ceiling((nComponents + 1)/2), 2), mar = rep(0, 4))
with(final.df[-c(1:window.size), ],
     plot(date, log_wheat_index_igc, type = "l",
          ylim = range(fitComponent, final.df$log_wheat_index_igc)))
for(i in 1:NCOL(fitComponent)){
    plot(final.df[window.size:NROW(final.df), "date"],
         fitComponent[, i], type = "l",
         ylim = range(fitComponent, final.df$log_wheat_index_igc))
}


## Show the rolling fit and the predictions
##
## NOTE (Michael): The rolling fit shows that with 16 variable, the
##                 fit is satisfactory.

fit =
    lapply(rollRelaxReg, function(x){
        data.frame(date = x$model$date, fit = fitted(x))
    })
    
pred30day =
    mapply(FUN = function(model, newdata){
        data.frame(date = newdata$date, pred = predict(model, newdata))
    },
    model = rollRelaxReg[-c((length(rollRelaxReg) - 29):length(rollRelaxReg))],
    newdata = lapply(rollRelaxReg, FUN = function(x) x$model)[-c(1:30)],
    SIMPLIFY = FALSE)

pred60day =
    mapply(FUN = function(model, newdata){
        data.frame(date = newdata$date, pred = predict(model, newdata))        
    },
    model = rollRelaxReg[-c((length(rollRelaxReg) - 59):length(rollRelaxReg))],
    newdata = lapply(rollRelaxReg, FUN = function(x) x$model)[-c(1:60)],
    SIMPLIFY = FALSE)
fullRange =
    range(sapply(fit, function(x) range(x$fit)),
          sapply(pred30day, function(x) range(x$pred)),
          sapply(pred60day, function(x) range(x$pred)))

par(mfrow = c(3, 1), mar = rep(0, 4))
## Rolling fit
with(final.df, plot(date, log_wheat_index_igc, lwd = 2, type = "n",
                    ylim = fullRange))
lapply(fit, function(x) with(x, lines(x$date, x$fit, col = "red")))
with(final.df, lines(date, log_wheat_index_igc, lwd = 5))

## rolling prediction for 30 days in the future
with(final.df, plot(date, log_wheat_index_igc, lwd = 2, type = "n",
                    ylim = fullRange))
lapply(pred30day, function(x) with(x, lines(x$date, x$pred, col = "red")))
with(final.df, lines(date, log_wheat_index_igc, lwd = 5))

## rolling prediction for 60 days in the future
with(final.df, plot(date, log_wheat_index_igc, lwd = 2, type = "n",
                    ylim = fullRange))
lapply(pred60day, function(x) with(x, lines(x$date, x$pred, col = "red")))
with(final.df, lines(date, log_wheat_index_igc, lwd = 5))





## Split the data in 100 fold, and fit regression to each
## foldSize = 260
## nFold = ceiling(NROW(final.df)/foldSize)

nFold = 30
foldSize = ceiling(NROW(final.df)/(nFold + 1))
init = 0
partition = vector("list", length = nFold)
for(i in 1:nFold){
    if(i == nFold){
        partition[[i]] = final.df[init:NROW(final.df), ]
    } else {
        partition[[i]] = final.df[(init + 1):(init + foldSize), ]
    }
    init = init + foldSize
}

partitionModel =
    lapply(partition,
           FUN = function(x){
               cv.glmnet(x = as.matrix(x[, -c(1, 2)]),
                         y = as.matrix(x[, 2]))
           })    

with(final.df, plot(date, log_wheat_index_igc, type = "l", lwd = 2))
mapply(FUN = function(x, y){
    lines(y[, 1],
          predict(x, newx = as.matrix(y[, -c(1, 2)])), col = "green", lty = 2)
}, x = partitionModel, y = partition)
mapply(FUN = function(x, y){
    lines(y[, 1],
          predict(x, newx = as.matrix(y[, -c(1, 2)])), col = "red", lty = 2)
}, x = partitionModel[-length(partitionModel)], y = partition[-1])
