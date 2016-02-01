## Load libraries
library(forecast)
library(quantmod)
library(glmnet)
library(caret)

## Read the wheat IGC data
wheat_igc.df = read.csv(file = "wheat_index_igc.csv")
wheat_igc.df$date = as.Date(wheat_igc.df$date, "%m/%d/%Y")

## Plot the data
with(wheat_igc.df,
{
    plot(date, wheat_index_igc, type = "l")
}
)


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
    na.omit(merge(wheat_igc =
                      as.xts(wheat_igc.df$wheat_index_igc,
                             order.by = wheat_igc.df$date),
                  exchange_rates))
final.df = as.data.frame(final_wheat.xts)
for(i in colnames(final.df)[2:NCOL(final.df)]){
    for(j in 1:31){
        new_name = paste0(i, "_lag", j)
        final.df[[new_name]] = lag(final.df[[i]], j)
    }
}

no_missing.df = na.omit(final.df)


## predictable equals to positive CCF

## TODO (Michael): Should create lagged data


## Split the data for training and testing
##
## CHECK (Michael): Check the partition function in caret

n = NROW(no_missing.df)
train_pct = 0.9
train.df = no_missing.df[1:(n * train_pct), ]
test.df = no_missing.df[(n * train_pct + 1):n, ]

## Create formula
wheat.formula =
    as.formula(paste0("wheat_igc ~ ",
                      paste0(gsub("/", ".", colnames(final.df)[-1]),
                             collapse = "+")))

## Arima Model
arima.fit =
    auto.arima(x = train.df[, 1],
               xreg = as.Date(rownames(train.df)))
arima.pred = predict(arima.fit, newxreg = as.Date(rownames(test.df)))$pred

## Lasso Model
lasso.fit = cv.glmnet(x = as.matrix(train.df[, -1]),
                      y = log(as.matrix(train.df[, 1])),
                      type.measure = "mse", nfolds = 10)
lasso.pred = predict(lasso.fit, newx = as.matrix(test.df[, -1]))


## Neural Nets
grid.nn = expand.grid(layer1 = 1:5, layer2 = 0, layer3 = 0)
control.nn = trainControl(number = 3)
nn.fit = train(wheat.formula,
               data = train.df, method = "neuralnet",
               tuneGrid = grid.nn, trControl = control.nn)
nn.pred = predict(nn.fit, test.df[, -1])


## Lasso, but the cv.glmnet seems to be much faster
## grid.lasso = expand.grid(fraction = seq(0, 1, length = 10))
## lasso.fit = train(wheat.formula, data = train.df,
##                   method = "lasso", tuneGrid = grid.lasso)
## lasso.pred = predict(lasso.fit, test.df[, -1])

plot(as.Date(rownames(final.df)), final.df[, 1],
     ylim = c(0, max(final.df[, 1])), type = "l")
lines(as.Date(rownames(train.df[, -1])), fitted(arima.fit), col = "red", lty = 2)
lines(as.Date(rownames(test.df[, -1])), arima.pred, col = "red", lty = 2)
lines(as.Date(rownames(train.df[, -1])),
      exp(predict(lasso.fit, newx = as.matrix(train.df[, -1]))),
      col = "steelblue", lty = 2)
lines(as.Date(rownames(test.df[, -1])), exp(lasso.pred),
      col = "steelblue", lty = 2)
lines(as.Date(rownames(test.df[, -1])), nn.pred, col = "green", lty = 2)


## lines(as.Date(rownames(train.df[, -1])),
##       predict(blasso.fit, as.data.frame(train.df[, -1])),
##       col = "orange", lty = 2)
## lines(as.Date(rownames(test.df[, -1])), blasso.pred, col = "orange", lty = 2)


