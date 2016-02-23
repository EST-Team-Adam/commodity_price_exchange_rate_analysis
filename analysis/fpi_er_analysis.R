library(glmnet)
library(quantmod)

## Load the wheat data and log transform the response
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

## Create lagged terms for the exchange rates, here we have created 60
## lags, or approximately 2 months.
for(i in exchange_rate_names){
    for(j in 1:260){
        new_name = paste0(i, "_lag", j)
        er.df[[new_name]] = c(rep(NA, j), er.df[[i]][(1:(NROW(er.df) - j))])
    }
}
## Create time
er.df$date = as.Date(rownames(er.df))

## Construct the final data frame
final.df = na.omit(merge(wheat_igc.df, er.df, by = "date", all = FALSE))

########################################################################
## Correlation Analysis
########################################################################


## Compute running correlation, the rolling window is 260, which
## corresponds to 260 days or approximately 1 year.
xsetname = colnames(final.df)[3:NCOL(final.df)]
windowSize = 260
runcor.lst = vector("list", length = length(xsetname))
for(i in 1:length(xsetname)){
    runcor.lst[[i]] = runCor(x = final.df[, "log_wheat_index_igc"],
                             y = final.df[, xsetname[i]],
                             n = windowSize)
}
runcor.df = as.data.frame(runcor.lst)
colnames(runcor.df) = xsetname

## Compute the variance of each running correlation
runcorvar = sapply(runcor.lst, var, na.rm = TRUE)
names(runcorvar) = xsetname

## Select the most stable running correlation with the smallest variance
stable_cor_list = vector("numeric", length(exchange_rate_names))
for(i in 1:length(exchange_rate_names)){
    cur_name = exchange_rate_names[i]
    all_lag_ind = grep(cur_name, names(runcorvar))
    stable_cor_ind = which.min(runcorvar[all_lag_ind])
    stable_cor_list[i] = runcorvar[all_lag_ind[stable_cor_ind]]
    names(stable_cor_list)[i] = names(runcorvar)[all_lag_ind[stable_cor_ind]]
}

## Plot the most stable running correlation for each currency
par(mfrow = c(7, 3), mar = c(0, 4, 0, 0))
for(i in names(stable_cor_list)){
    plot(final.df$date,
         runcor.df[[i]], type = "l", ylab = i, ylim = c(-1, 1))
    ## abline(h = mean(runcor.df[[i]], na.rm = TRUE), col = "blue", lty = 2)
    abline(h = 0, col = "red", lty = 2)
}

## Compute the overall correlation
overallcor = cor(final.df[, 2], final.df[, -c(1, 2)],
                 use = "pairwise.complete.obs")
colnames(overallcor)[which.max(abs(overallcor))]
max(abs(overallcor))

## Plot the currency with the highest overall correlation with the wheat price
maxCovVar = colnames(overallcor)[which.max(abs(overallcor))]
par(mfrow = c(4, 1))
plot(final.df[, "log_wheat_index_igc"], type = "l")
plot(final.df[, maxCovVar], type = "l")
plot(final.df[, maxCovVar],
     final.df[, "log_wheat_index_igc"],
     type = "l",
     xlab = "Max correlation currency",
     ylab = "Logged Wheat Index")
plot(runcor.df[, maxCovVar], type = "l", ylim = c(-1, 1))
abline(h = 0, col = "red", lty = 2)

max_cor_list = vector("numeric", length(exchange_rate_names))
## Select the maximum correlation of each currency
for(i in 1:length(exchange_rate_names)){
    cur_name = exchange_rate_names[i]
    all_lag_ind = grep(cur_name, colnames(overallcor))
    max_cor_ind = which.max(abs(overallcor[all_lag_ind]))
    max_cor_list[i] = overallcor[all_lag_ind[max_cor_ind]]
    names(max_cor_list)[i] = colnames(overallcor)[all_lag_ind[max_cor_ind]]
}


par(mfrow = c(7, 3), mar = c(0, 0, 0, 0))
for(i in names(max_cor_list)){
    plot(final.df$log_wheat_index_igc, final.df[[i]],
         ylab = i, pch = 19, col = rgb(0, 0, 0, alpha = 0.1), axes = FALSE)
    legend("topright", i, bty = "n")
    box()
}


## NOTE (Michael): Should show a plot of the Pakistan to illustrate
##                 why the overall correlation is not a good measure,
##                 and rolling correlation is better.

## NOTE (Michael): The correlation analysis shows that there are
##                 indeed correlation over very long periods (that is,
##                 trend). But the short run correlation is very weak
##                 if non-existent.



########################################################################
## Predictive Analysis
########################################################################

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
