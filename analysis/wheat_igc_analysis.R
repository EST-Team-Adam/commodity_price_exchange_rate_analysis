## Load libraries
library(forecast)
library(quantmod)
library(glmnet)
library(caret)
## registerDoMC(core = 2)

## Read the wheat IGC data
wheat_igc.df = read.csv(file = "wheat_index_igc.csv")
wheat_igc.df$date = as.Date(wheat_igc.df$date, "%m/%d/%Y")

## Plot the data
## with(wheat_igc.df,
## {
##     plot(date, wheat_index_igc, type = "l")
## }
## )


## Oil Future price
oil_future.df = read.csv(file = "oil_future.csv", stringsAsFactor = FALSE)
oil_future.df$Date = as.Date(oil_future.df$Date)

gas_future.df = read.csv(file = "gas_future.csv", stringsAsFactor = FALSE)
gas_future.df$Date = as.Date(gas_future.df$Date)

## Get exchange rate data
exchange_rate_basket =
    paste0("USD/", c("EUR", "JPY", "GBP", "CHF", "CAD", "AUD", "NZD", "ZAR",
                     "HKD", "INR", "RUB", "PKR", "UAH", "TRY"))




get_all_currency = function(basket, from, to){
    n_currency = length(basket)
    currency_basket <<- vector(mode = "list", length = n_currency)

    for(i in 1:n_currency){
        if((to - from) < 500){
            currency_basket[[i]] =
                getFX(basket[i], from = from, to = to, auto.assign = FALSE)
        } else {
            split_dates = unique(c(seq(from - 1, to, by = 500), to))
            for(j in 1:(length(split_dates) - 1)){
                tmp = getFX(basket[i], from = split_dates[j] + 1,
                            to = split_dates[j + 1], auto.assign = FALSE)
                currency_basket[[i]] = rbind(currency_basket[[i]], tmp)
            }
        }
    }
    Reduce(function(x, y) merge(x, y), x = currency_basket)
}    




exchange_rates =
    get_all_currency(basket = exchange_rate_basket,
                     from = min(wheat_igc.df$date),
                     to = max(wheat_igc.df$date))


final_wheat.xts =
    merge(wheat_igc =
              as.xts(wheat_igc.df$wheat_index_igc,
                     order.by = wheat_igc.df$date),
          exchange_rates)
wheat_oil.xts =
    merge(final_wheat.xts,
          oil_future =
              as.xts(oil_future.df$Open,
                     order.by = oil_future.df$Date)
          )
wheat_oil_gas.xts =
    merge(wheat_oil.xts,
          gas_future =
              as.xts(gas_future.df$Open,
                     order.by = gas_future.df$Date)
          )

final.df = as.data.frame(na.omit(wheat_oil_gas.xts))
exchange_rate_basket_name = gsub("/", ".", exchange_rate_basket)

## Add lag terms
for(i in colnames(final.df)[-1]){
    for(j in 1:31){
        new_name = paste0(i, "_lag", j)        
        final.df[[new_name]] = c(rep(NA, j), final.df[[i]][(1:(NROW(final.df) - j))])
    }
}

## Add square terms
## for(i in exchange_rate_basket_name){
##     new_name = paste0(i, "_squared")
##     final.df[[new_name]] = final.df[[i]]^2
## }

## Add volatility
## final.df$lag_vol = c(NA, diff(log(final.df$wheat_igc)))

no_missing.df = na.omit(final.df)


## predictable equals to positive CCF

## TODO (Michael): Should create lagged data


## Split the data for training and testing
##
## CHECK (Michael): Check the partition function in caret

n = NROW(no_missing.df)
train_pct = 0.8
cut_point = round(n * train_pct)
train.df = no_missing.df[1:cut_point, ]
test.df = no_missing.df[(cut_point + 1):n, ]

## Create formula
wheat.formula =
    as.formula(paste0("wheat_igc ~ -1 + oil_future + gas_future + ",
                      paste0(gsub("/", ".", colnames(final.df)[-1]),
                             collapse = "+")))

## Arima Model
arima.fit =
    auto.arima(x = train.df[, 1],
               xreg = as.Date(rownames(train.df)))
arima.pred = predict(arima.fit, newxreg = as.Date(rownames(test.df)))$pred

## Lasso Model
lasso.fit = cv.glmnet(x = as.matrix(train.df[, -1]),
                      y = as.matrix(train.df[, 1]),
                      type.measure = "mse", nfolds = 20)
lasso.pred = predict(lasso.fit, newx = as.matrix(test.df[, -1]))


## Neural Nets
grid.nn = expand.grid(layer1 = c(1, 3, 5, 10), layer2 = 0, layer3 = 0)
control.nn = trainControl(number = 3)
## nn.fit = train(wheat.formula,
##                data = train.df, method = "neuralnet",
##                ## tuneGrid = grid.nn, trControl = control.nn,
##                preProcess = c("center", "scale"))
## library(nnet)
## nn.fit = nnet(wheat.formula, data = train.df, size = c(5, 5), MaxNWts = 2500)
## nn.fit = neuralnet(wheat.formula, hidden = c(5, 5), data = train.df,
##                    learningrate = 0.01, threshold = 1e-5)
## nn.pred = compute(nn.fit, test.df[, -1])$net.result
## nn.pred = predict(nn.fit, test.df)

## Linear regression
lm.fit = lm(wheat.formula, data = train.df)
lm.pred = predict(lm.fit, newdata = test.df)


## Lasso, but the cv.glmnet seems to be much faster
## grid.lasso = expand.grid(fraction = seq(0, 1, length = 10))
## lasso.fit = train(wheat.formula, data = train.df,
##                   method = "lasso", tuneGrid = grid.lasso)
## lasso.pred = predict(lasso.fit, test.df[, -1])

plot(as.Date(rownames(final.df)), final.df[, 1],
     ylim = c(0, max(final.df[, 1])), type = "l") 
     ## ylim = c(0, 1000), type = "l")
lines(as.Date(rownames(train.df[, -1])), fitted(arima.fit), col = "red", lty = 2)
lines(as.Date(rownames(test.df[, -1])), arima.pred, col = "red", lty = 2)
lines(as.Date(rownames(train.df[, -1])),
      predict(lasso.fit, newx = as.matrix(train.df[, -1])),
      col = "steelblue", lty = 2)
lines(as.Date(rownames(test.df[, -1])), lasso.pred,
      col = "steelblue", lty = 2)
## lines(as.Date(rownames(train.df[, -1])),
##       compute(nn.fit, train.df[, -1])$net.result, col = "green", lty = 2)
## lines(as.Date(rownames(test.df[, -1])), nn.pred, col = "green", lty = 2)
lines(as.Date(rownames(train.df[, -1])),
      predict(lm.fit, newdata = train.df[, -1]), col = "violet", lty = 2)
lines(as.Date(rownames(test.df[, -1])), lm.pred, col = "violet", lty = 2)
## lines(as.Date(rownames(train.df[, -1])),
##       predict(lm.fit2, newdata = train.df[, -1]), col = "orange", lty = 2)
## lines(as.Date(rownames(test.df[, -1])), lm.pred2, col = "orange", lty = 2)



## plot(as.Date(rownames(final.df)), final.df[, 1],
##      ylim = c(0, max(final.df[, 1])), type = "l") 
##      ## ylim = c(0, 1000), type = "l")
## lines(as.Date(rownames(train.df[, -1])), exp(fitted(arima.fit)), col = "red", lty = 2)
## lines(as.Date(rownames(test.df[, -1])), exp(arima.pred), col = "red", lty = 2)
## lines(as.Date(rownames(train.df[, -1])),
##       exp(predict(lasso.fit, newx = as.matrix(train.df[, -1]))),
##       col = "steelblue", lty = 2)
## lines(as.Date(rownames(test.df[, -1])), exp(lasso.pred),
##       col = "steelblue", lty = 2)
## ## lines(as.Date(rownames(train.df[, -1])),
## ##       compute(nn.fit, train.df[, -1])$net.result, col = "green", lty = 2)
## ## lines(as.Date(rownames(test.df[, -1])), nn.pred, col = "green", lty = 2)
## lines(as.Date(rownames(train.df[, -1])),
##       exp(predict(lm.fit, newdata = train.df[, -1])), col = "violet", lty = 2)
## lines(as.Date(rownames(test.df[, -1])), exp(lm.pred), col = "violet", lty = 2)
## ## lines(as.Date(rownames(train.df[, -1])),
## ##       predict(lm.fit2, newdata = train.df[, -1]), col = "orange", lty = 2)
## ## lines(as.Date(rownames(test.df[, -1])), lm.pred2, col = "orange", lty = 2)


plot(as.zoo(no_missing.df))
plot(as.zoo(final_wheat.xts))

plot(final.df$wheat_igc, c(exp(predict(lasso.fit, newx = as.matrix(train.df[, -1]))), exp(lasso.pred)), ylim = c(0, 500), xlim = c(0, 500))
abline(0, 1, col = "red", lty = 2)


lm.fit2 = lm(wheat_igc ~ poly(USD.AUD, 2), data = train.df)
lm.pred2 = predict(lm.fit2, newdata = test.df)


plot(as.data.frame(final_wheat.xts))


lm.fit2 = lm(USD.AUS ~ wheat_igc, data = train.df)
lm.pred2 = predict(lm.fit2, newdata = test.df)
par(mfrow = c(2, 1))
plot(final.df$wheat_igc, type = "l")
plot(as.Date(rownames(final.df)), final.df$USD.EUR, type = "l", ylim = c(0.5, 1.5))
lines(as.Date(rownames(train.df[, -1])),
      predict(lm.fit2, newdata = train.df), col = "orange", lty = 2)
lines(as.Date(rownames(test.df)), lm.pred2, col = "red", lty = 2)

lines(c(fitted(lm.fit2), lm.pred2), col = "red")




n = NROW(no_missing.df)
train_pct = 0.6
cut_point = round(n * train_pct)
train.df = no_missing.df[1:cut_point, ]
test.df = no_missing.df[(cut_point + 1):n, ]

## Testing with Granger's causality
arima.fit = auto.arima(x = log(train.df[, 1]))
arima.pred = predict(arima.fit, n.ahead = NROW(test.df))$pred

varima.fit = auto.arima(x = log(train.df[, 1]), xreg = train.df[, -1])
varima.pred = predict(varima.fit, newxreg = test.df[, -1])$pred

plot(as.Date(rownames(final.df)), final.df$wheat_igc, type = "l")
lines(as.Date(rownames(final.df)),
      exp(c(fitted(arima.fit), arima.pred)), col = "red")
lines(as.Date(rownames(final.df)),
      exp(c(fitted(varima.fit), varima.pred)), col = "green")

sse = function(y, yhat){
    sum((y - yhat)^2)
}

sse(test.df$wheat_igc, arima.pred)
sse(test.df$wheat_igc, varima.pred)

## NOTE (Michael): According to the test, the exchange rate does add
##                 predictability, however the question is how much.

## url = "http://www.quandl.com/api/v1/datasets/CHRIS/CME_CL1.csv"
## oilFut = read.csv(file = url)


with(no_missing.df,
{
     plot(as.Date(rownames(no_missing.df)), wheat_igc, type = "l",
          ylim = c(0, 450))
     lines(as.Date(rownames(no_missing.df)), oil_future, col = "red", lty = 2)
})


wheat_future.df = no_missing.df[, c("oil_future", "wheat_igc")]

for(i in 1:150){
    new_name = paste0("wheat_lag", i)
    wheat_future.df[[new_name]] = c(rep(NA, i), wheat_future.df[["wheat_igc"]][(1:(NROW(wheat_future.df) - i))])
}
wheat_oil_final.df = na.omit(wheat_future.df)

n = NROW(wheat_oil_final.df)
train_pct = 0.8
cut_point = round(n * train_pct)
train.df = wheat_oil_final.df[1:cut_point, ]
test.df = wheat_oil_final.df[(cut_point + 1):n, ]

## test.lm = lm(oil_future ~ ., data = train.df)

lasso.fit = cv.glmnet(x = as.matrix(train.df[, -1]),
                      y = as.matrix(train.df[, 1]),
                      type.measure = "mse", nfolds = 20)
lasso.pred = predict(lasso.fit, newx = as.matrix(test.df[, -1]))

test_formula = as.formula(paste0("oil_future ~ ", paste0(colnames(train.df)[-1], collapse = " + ")))
nn.fit = train(test_formula,
               data = train.df, method = "neuralnet")
nn.pred = predict(nn.fit, test.df)

with(no_missing.df,
{
     plot(as.Date(rownames(no_missing.df)), oil_future, type = "l",
          ylim = c(0, 300))
     ## lines(as.Date(rownames(train.df)), fitted(test.lm), col = "green", lty = 2)
     ## lines(as.Date(rownames(test.df)), predict(test.lm, test.df), col = "red", lty = 2)
     lines(as.Date(rownames(train.df)), predict(lasso.fit, newx = as.matrix(train.df[, -1])), col = "green", lty = 2)
     lines(as.Date(rownames(test.df)), lasso.pred, col = "red", lty = 2)
     ## lines(as.Date(rownames(train.df)), fitted(nn.fit), col = "green", lty = 2)
     ## lines(as.Date(rownames(test.df)), nn.pred, col = "red", lty = 2)    
})

















wheat_usd.df = no_missing.df[, c("wheat_igc", "USD.EUR")]

for(i in 1:150){
    new_name = paste0("usd_lag", i)
    wheat_usd.df[[new_name]] = c(rep(NA, i), wheat_usd.df[["USD.EUR"]][(1:(NROW(wheat_usd.df) - i))])
}
wheat_oil_final.df = na.omit(wheat_usd.df)

n = NROW(wheat_oil_final.df)
train_pct = 0.3
cut_point = round(n * train_pct)
train.df = wheat_oil_final.df[1:cut_point, ]
test.df = wheat_oil_final.df[(cut_point + 1):n, ]

## test.lm = lm(oil_future ~ ., data = train.df)

lasso.fit = cv.glmnet(x = as.matrix(train.df[, -1]),
                      y = as.matrix(train.df[, 1]),
                      type.measure = "mse", nfolds = 20)
lasso.pred = predict(lasso.fit, newx = as.matrix(test.df[, -1]))

with(no_missing.df,
{
     plot(as.Date(rownames(no_missing.df)), wheat_igc, type = "l")
     ## lines(as.Date(rownames(train.df)), fitted(test.lm), col = "green", lty = 2)
     ## lines(as.Date(rownames(test.df)), predict(test.lm, test.df), col = "red", lty = 2)
     lines(as.Date(rownames(train.df)), predict(lasso.fit, newx = as.matrix(train.df[, -1])), col = "green", lty = 2)
     lines(as.Date(rownames(test.df)), lasso.pred, col = "red", lty = 2)
     ## lines(as.Date(rownames(train.df)), fitted(nn.fit), col = "green", lty = 2)
     ## lines(as.Date(rownames(test.df)), nn.pred, col = "red", lty = 2)    
})









eur.df = no_missing.df[, c("wheat_igc",
                           grep("EUR", colnames(no_missing.df), value = TRUE))]
n = NROW(eur.df)
train_pct = 0.8
cut_point = round(n * train_pct)
train.df = eur.df[1:cut_point, ]
test.df = eur.df[(cut_point + 1):n, ]

## Testing with Granger's causality
arima.fit = auto.arima(x = log(train.df[, 1]))
arima.pred = predict(arima.fit, n.ahead = NROW(test.df))$pred

varima.fit = auto.arima(x = log(train.df[, 1]), xreg = train.df[, -1])
varima.pred = predict(varima.fit, newxreg = test.df[, -1])$pred

plot(as.Date(rownames(eur.df)), eur.df$wheat_igc, type = "l")
lines(as.Date(rownames(eur.df)),
      exp(c(fitted(arima.fit), arima.pred)), col = "red")
lines(as.Date(rownames(eur.df)),
      exp(c(fitted(varima.fit), varima.pred)), col = "green")
