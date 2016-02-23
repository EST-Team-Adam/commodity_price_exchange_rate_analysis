library(glmnet)
library(quantmod)

wheat_igc.df = read.csv(file = "wheat_index_igc.csv")
wheat_igc.df$date = as.Date(wheat_igc.df$date, "%m/%d/%Y")
wheat_igc.df$log_wheat_index_igc = log(wheat_igc.df$wheat_index_igc)
wheat_igc.df$wheat_index_igc = NULL

## Get exchange rate data
exchange_rate_basket =
    paste0("USD/", c("EUR", "JPY", "GBP", "CHF", "CAD", "AUD", "NZD", "ZAR",
                     "HKD", "INR", "RUB", "PKR", "UAH", "TRY"))




get_all_currency = function(basket, from, to){
    n_currency = length(basket)
    currency_basket = vector(mode = "list", length = n_currency)

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
    data.frame(Reduce(function(x, y) merge(x, y), x = currency_basket))
}    




exchange_rate_names = gsub("/", ".", exchange_rate_basket)
er.df =
    get_all_currency(basket = exchange_rate_basket,
                     from = min(wheat_igc.df$date),
                     to = max(wheat_igc.df$date))

## Create lagged terms
for(i in exchange_rate_names){
    for(j in 1:31){
        new_name = paste0(i, "_lag", j)
        er.df[[new_name]] = c(rep(NA, j), er.df[[i]][(1:(NROW(er.df) - j))])
    }
}

## Creating interaction term
er.df$USD.EUR.JPY = er.df$USD.EUR * er.df$USD.JPY
er.df$USD.EUR.GBP = er.df$USD.EUR * er.df$USD.GBP
er.df$date = as.Date(rownames(er.df))


final.df = merge(wheat_igc.df, er.df, by = "date", all = FALSE)
## final.df = final.df[1500:NROW(final.df), ]

for(i in 1:31){
    new_name = paste0("wheat_lag", i)
    final.df[[new_name]] = c(rep(NA, i), final.df[["log_wheat_index_igc"]][(1:(NROW(final.df) - i))])
}

no_missing.df = na.omit(final.df[final.df$date >= as.Date("2000-01-01"), ])
no_missing.df$dummy = ifelse(no_missing.df$date <= as.Date("2010-01-01"), 1, 0)
                                      
n = NROW(no_missing.df)
train_pct = 0.5
cut_point = round(n * train_pct)
train.df = no_missing.df[1:cut_point, ]
test.df = no_missing.df[(cut_point + 1):n, ]
full_time = no_missing.df$date
train_time = train.df$date
test_time = test.df$date

## Create formula
lasso.fit = cv.glmnet(x = as.matrix(train.df[, 3:16]),
                      y = as.matrix(train.df[, 2]),
                      type.measure = "mse", nfolds = 10)
lasso.pred = predict(lasso.fit, newx = as.matrix(test.df[, 3:16]))
lasso.fitted = predict(lasso.fit, newx = as.matrix(train.df[, 3:16]))

## lasso.fit = cv.glmnet(x = as.matrix(train.df[, -c(1, 2)]),
##                       y = as.matrix(train.df[, 2]),
##                       type.measure = "mse", nfolds = 10)
## lasso.pred = predict(lasso.fit, newx = as.matrix(test.df[, -c(1, 2)]))
## lasso.fitted = predict(lasso.fit, newx = as.matrix(train.df[, -c(1, 2)]))

## dummy.fit = cv.glmnet(x = as.matrix(train.df[, 3:452]),
##                       y = as.matrix(train.df[, 2]),
##                       type.measure = "mse", nfolds = 10)
## dummy.pred = predict(dummy.fit, newx = as.matrix(test.df[, 3:452]))
## dummy.fitted = predict(dummy.fit, newx = as.matrix(train.df[, 3:452]))


## par(mfrow = c(4, 1))
plot(full_time, no_missing.df$log_wheat_index_igc, type = "l", ylim = c(4, 7))
lines(train_time, lasso.fitted, col = "green", lty = 2)
lines(test_time, lasso.pred,  col = "red", lty = 2)
lines(test_time, dummy.pred,  col = "steelblue", lty = 2)
## plot(no_missing.df$date, no_missing.df$EUR.WHEAT.COR, type = "l")
## plot(no_missing.df$date, no_missing.df$JPY.WHEAT.COR, type = "l")
## plot(no_missing.df$date, no_missing.df$GBP.WHEAT.COR, type = "l")
