library(glmnet)
library(quantmod)

wheat_igc.df = read.csv(file = "wheat_index_igc.csv")
wheat_igc.df$date = as.Date(wheat_igc.df$date, "%m/%d/%Y")
wheat_igc.df$log_wheat_index_igc = log(wheat_igc.df$wheat_index_igc)
wheat_igc.df$wheat_index_igc = NULL

## Get exchange rate data
top10currencies = c("EUR", "JPY", "GBP", "CHF", "CAD", "MXN", "NZD")
top10producingCurrencies = c("INR", "RUB", "PKR", "AUD", "UAH")
top10tradingCurrencies = c("KZT", "TRY", "BRL", "EGP", "IDR", "DZD", "PHP",
                           "NGN", "KRW")
exchange_rate_basket = 
    paste0("USD/", c(top10currencies, top10producingCurrencies,
                     top10tradingCurrencies))
exchange_rate_names = gsub("/", ".", exchange_rate_basket)


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





er.df =
    get_all_currency(basket = exchange_rate_basket,
                     from = min(wheat_igc.df$date),
                     to = max(wheat_igc.df$date))

## Create lagged terms
for(i in exchange_rate_names){
    for(j in 1:60){
        new_name = paste0(i, "_lag", j)
        er.df[[new_name]] = c(rep(NA, j), er.df[[i]][(1:(NROW(er.df) - j))])
    }
}

## ## Creating interaction term
## er.df$USD.EUR.JPY = er.df$USD.EUR * er.df$USD.JPY
## er.df$USD.EUR.GBP = er.df$USD.EUR * er.df$USD.GBP

## Create time
er.df$date = as.Date(rownames(er.df))

## nymex_oil_future.df = read.csv(file = "nymex_oil_future.csv", stringsAsFactor = FALSE)
bp_oil.df = read.csv(file = "bp_oil.csv", stringsAsFactor = FALSE)
bp_oil.df$date = as.Date(bp_oil.df$Date)
bp_oil.df$Date = NULL
colnames(bp_oil.df)[1:5] = paste0(colnames(bp_oil.df)[1:5], ".bp")
wti_oil_crude.df = read.csv(file = "wti_oil_crude.csv", stringsAsFactor = FALSE)
wti_oil_crude.df$date = as.Date(wti_oil_crude.df$Date)
wti_oil_crude.df$Date = NULL
colnames(wti_oil_crude.df) = c("Value.crude", "date")

oil.df = merge(bp_oil.df, wti_oil_crude.df, by = "date")


## oil_future.df = merge(nymex_oil_future.df, wti_oil_future.df, by = "Date")
## colnames(oil_future.df) = gsub("\\.x", ".nymex", colnames(oil_future.df))
## colnames(oil_future.df) = gsub("\\.y", ".wti", colnames(oil_future.df))

oil_er.df = merge(oil.df[, c("date", "Open.bp", "Value.crude")], er.df,
                  by = "date", all = FALSE)

final.df = merge(wheat_igc.df, oil_er.df, by = "date", all = FALSE)
for(i in 1:31){
    new_name = paste0("wheat_lag", i)
    final.df[[new_name]] = c(rep(NA, i), final.df[["log_wheat_index_igc"]][(1:(NROW(final.df) - i))])
}



final.df$highCor = ifelse(with(final.df, runCor(USD.EUR, log_wheat_index_igc, n = 365)) <= -0.5, "green", "red")

par(mfrow = c(4, 1), mar = c(0, 4, 0, 0))
with(final.df, plot(USD.EUR, log_wheat_index_igc, pch = 19, col = highCor))
with(final.df, plot(date, USD.EUR/max(USD.EUR), type = "l", ylim = c(0, 1)))
with(final.df, lines(date, log_wheat_index_igc/max(log_wheat_index_igc), col = "red", lty = 2))
with(final.df, lines(date, Value.crude/max(Value.crude), col = "green", lty = 2))



## with(final.df, lines(date, Open.bp/max(Open.bp), col = "steelblue", lty = 2))
plot(with(final.df, runCor(log_wheat_index_igc, USD.EUR, n = 183)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
plot(with(final.df, runCor(log_wheat_index_igc, Value.crude, n = 183)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
## plot(with(final.df, runCor(log_wheat_index_igc, Open.bp, n = 183)), type = "l", ylim = c(-1, 1))
## abline(h = 0, col = "red", lty = 2)
## plot(with(final.df, runCor(USD.EUR, Open.bp, n = 183)), type = "l", ylim = c(-1, 1))
## abline(h = 0, col = "red", lty = 2)


decomposition = stl(ts(final.df$log_wheat_index_igc, frequency = 365),
                    s.window = "periodic")
final.df$wheat_trend = decomposition$time.series[, 2]
final.df$wheat_res = decomposition$time.series[, 3]


n = NROW(final.df)
train_pct = 0.6
cut_point = round(n * train_pct)
train.df = final.df[1:cut_point, ]
test.df = final.df[(cut_point + 1):n, ]
full_time = final.df$date
train_time = train.df$date
test_time = test.df$date


y = train.df$log_wheat_index_igc
x = bs(train.df$Value.crude)

test.fit = lm(wheat_trend~ bs(Value.crude) + bs(USD.EUR) + bs(USD.JPY),
              data = train.df)
plot(full_time, exp(final.df$log_wheat_index_igc), type = "l")
lines(train_time, exp(fitted(test.fit)), col = "green")
lines(test_time, exp(predict(test.fit, newdata = test.df)), col = "red")
plot(full_time, exp(final.df$wheat_trend), type = "l")
lines(train_time, exp(fitted(test.fit)), col = "green")
lines(test_time, exp(predict(test.fit, newdata = test.df)), col = "red")


lasso.fit = cv.glmnet(x = with(train.df,
                               cbind(bs(Value.crude), bs(USD.EUR), bs(USD.AUD))),
                      ## y = train.df$wheat_trend,
                      y = train.df$log_wheat_index_igc,
                      type.measure = "mse", nfolds = 10)
plot(full_time, exp(final.df$log_wheat_index_igc), type = "l")
lines(train_time,
      exp(predict(lasso.fit,
              newx = with(train.df,
                             cbind(bs(Value.crude), bs(USD.EUR), bs(USD.AUD))))),
      col = "green")
lines(test_time,
      exp(predict(lasso.fit,
              newx = with(test.df,
                             cbind(bs(Value.crude), bs(USD.EUR), bs(USD.AUD))))),
      col = "red")
plot(full_time, exp(final.df$wheat_trend), type = "l")
lines(train_time,
      exp(predict(lasso.fit,
              newx = with(train.df,
                             cbind(bs(Value.crude), bs(USD.EUR), bs(USD.AUD))))),
      col = "green")
lines(test_time,
      exp(predict(lasso.fit,
              newx = with(test.df,
                             cbind(bs(Value.crude), bs(USD.EUR), bs(USD.AUD))))),
      col = "red")





plot(full_time, exp(final.df$log_wheat_index_igc), type = "l", ylim = c(0, 500))
lines(train_time,
      exp(predict(lasso.fit,
              newx = with(train.df,
                             cbind(bs(Value.crude), 0, 0, 0, 0, 0, 0)))),
      col = "red")
lines(train_time,
      exp(predict(lasso.fit,
              newx = with(train.df,
                             cbind(0, 0, 0, bs(USD.EUR), 0, 0, 0)))),
      col = "blue")
lines(train_time,
      exp(predict(lasso.fit,
              newx = with(train.df,
                             cbind(0, 0, 0, 0, 0, 0, bs(USD.AUD))))),
      col = "green")
lines(train_time,
      exp(predict(lasso.fit,
              newx = with(train.df,
                             cbind(bs(Value.crude), bs(USD.EUR), bs(USD.AUD))))),
      col = "steelblue")
lines(test_time,
      exp(predict(lasso.fit,
              newx = with(test.df,
                             cbind(bs(Value.crude), 0, 0, 0, 0, 0, 0)))),
      col = "red")
lines(test_time,
      exp(predict(lasso.fit,
              newx = with(test.df,
                             cbind(0, 0, 0, bs(USD.EUR), 0, 0, 0)))),
      col = "blue")
lines(test_time,
      exp(predict(lasso.fit,
              newx = with(test.df,
                             cbind(0, 0, 0, 0, 0, 0, bs(USD.AUD))))),
      col = "green")
lines(test_time,
      exp(predict(lasso.fit,
              newx = with(test.df,
                             cbind(bs(Value.crude), bs(USD.EUR), bs(USD.AUD))))),
      col = "steelblue")


y = train.df$log_wheat_index_igc
x = bs(train.df$Value.crude)
test.fit = lm(log_wheat_index_igc ~ bs(Value.crude, df = 10) + bs(USD.EUR, df = 10), data = train.df)
plot(full_time, exp(final.df$log_wheat_index_igc), type = "l")
lines(train_time, exp(fitted(test.fit)), col = "green")
lines(test_time, exp(predict(test.fit, newdata = test.df)), col = "red")








plot(with(final.df, runCor(USD.EUR, wheat_lag1, n = 365)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
plot(with(final.df, runCor(USD.EUR, wheat_lag2, n = 365)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
plot(with(final.df, runCor(USD.EUR, wheat_lag3, n = 365)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
plot(with(final.df, runCor(USD.EUR, wheat_lag4, n = 365)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
plot(with(final.df, runCor(USD.EUR, wheat_lag5, n = 365)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
plot(with(final.df, runCor(USD.EUR, wheat_lag6, n = 365)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
plot(with(final.df, runCor(USD.EUR, wheat_lag7, n = 365)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
plot(with(final.df, runCor(USD.EUR, wheat_lag8, n = 365)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
plot(with(final.df, runCor(USD.EUR, wheat_lag9, n = 365)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
plot(with(final.df, runCor(USD.EUR, wheat_lag10, n = 365)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
plot(with(final.df, runCor(USD.EUR, wheat_lag11, n = 365)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
plot(with(final.df, runCor(USD.EUR, wheat_lag12, n = 365)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
plot(with(final.df, runCor(USD.EUR, wheat_lag13, n = 365)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
plot(with(final.df, runCor(USD.EUR, wheat_lag14, n = 365)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)
plot(with(final.df, runCor(USD.EUR, wheat_lag15, n = 365)), type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)



par(mfrow = c(5, 2), mar = rep(0, 4))
with(final.df, plot(diff(log_wheat_index_igc), diff(USD.JPY), type = "l"))
plot(with(final.df, runCor(diff(log_wheat_index_igc), diff(USD.JPY), n = 60)), type = "l")
plot(with(final.df, runCor(diff(log_wheat_index_igc), diff(USD.JPY_lag1), n = 60)), type = "l")
plot(with(final.df, runCor(diff(log_wheat_index_igc), diff(USD.JPY_lag2), n = 60)), type = "l")
plot(with(final.df, runCor(diff(log_wheat_index_igc), diff(USD.JPY_lag3), n = 60)), type = "l")
plot(with(final.df, runCor(diff(log_wheat_index_igc), diff(USD.JPY_lag4), n = 60)), type = "l")
plot(with(final.df, runCor(diff(log_wheat_index_igc), diff(USD.JPY_lag5), n = 60)), type = "l")
plot(with(final.df, runCor(diff(log_wheat_index_igc), diff(USD.JPY_lag6), n = 60)), type = "l")
plot(with(final.df, runCor(diff(log_wheat_index_igc), diff(USD.JPY_lag7), n = 60)), type = "l")
plot(with(final.df, runCor(diff(log_wheat_index_igc), diff(USD.JPY_lag31), n = 60)), type = "l")

