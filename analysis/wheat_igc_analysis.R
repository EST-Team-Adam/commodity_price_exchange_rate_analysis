## Load libraries
library(forecast)
library(quantmod)
library(glmnet)

## Read the data
wheat_igc.df = read.csv(file = "wheat_index_igc.csv")
wheat_igc.df$date = as.Date(wheat_igc.df$date, "%m/%d/%Y")

## Plot the data
with(wheat_igc.df,
{
    plot(date, wheat_index_igc, type = "l")
}
)

## Split the data
n = NROW(wheat_igc.df)
train_pct = 0.9
wheat_igc_train.df = wheat_igc.df[1:(n * train_pct), ]
wheat_igc_test.df = wheat_igc.df[(n * train_pct+ 1):n, ]

## Test auto.arima
##
## NOTE (Michael): For simplicity, we will ignore the unevenly spaced
##                 intervals.

wigc.model =
    auto.arima(x = wheat_igc_train.df$wheat_index_igc,
               xreg = wheat_igc_train.df$date)


with(wheat_igc.df,
{
    plot(date, wheat_index_igc, type = "l")
    pred = predict(wigc.model, newxreg = wheat_igc_test.df$date)$pred
    lines(wheat_igc_test.df$date, pred, lty = 2, col = "red")
}
)


## Get exchange rate data
exchange_rate_basket =
    paste0("USD/", c("EUR", "JPY", "GBP", "CHF", "CAD", "AUD", "NZD", "ZAR",
                     "RMB", "INR", "RUB", "PKR", "UAH", "TRY"))




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


final_wheat.xts = merge(wheat_igc =
                            as.xts(wheat_igc.df$wheat_index_igc,
                                   order.by = wheat_igc.df$date),
                        exchange_rates)

cor(na.omit(as.data.frame(final_wheat.xts)))

## Post processing data
final.mat = as.matrix(na.omit(final_wheat.xts))
n = NROW(final.mat)
train_pct = 0.9
train.mat = final.mat[1:(n * train_pct), ]
test.mat = final.mat[(n * train_pct + 1):n, ]

## Arima Model
arima.fit =
    auto.arima(x = train.mat[, 1],
               xreg = as.Date(rownames(train.mat)))
arima.pred = predict(arima.fit, newxreg = as.Date(rownames(test.mat)))$pred

## Lasso Model
lasso.fit = cv.glmnet(x = train.mat[, -1], y = log(train.mat[, 1]),
                      type.measure = "mse", nfolds = 50)
lasso.pred = predict(lasso.fit, newx = test.mat[, -1])


plot(as.Date(rownames(final.mat)), final.mat[, 1],
     ylim = c(0, max(final.mat[, 1])), type = "l")
lines(as.Date(rownames(train.mat[, -1])), fitted(arima.fit), col = "red", lty = 2)
lines(as.Date(rownames(test.mat[, -1])), arima.pred, col = "red", lty = 2)
lines(as.Date(rownames(train.mat[, -1])), exp(predict(lasso.fit, newx = train.mat[, -1])), col = "steelblue", lty = 2)
lines(as.Date(rownames(test.mat[, -1])), exp(lasso.pred),
      col = "steelblue", lty = 2)
