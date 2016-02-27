########################################################################
## Title: This Script extracts and process the wheat price index and
## exchange rates.
########################################################################

library(quantmod)

## Load the wheat data and log transform the response
wheat_igc.df = read.csv(file = "wheat_index_igc.csv")
wheat_igc.df$date = as.Date(wheat_igc.df$date, "%m/%d/%Y")
wheat_igc.df$log_wheat_index_igc = log(wheat_igc.df$wheat_index_igc)
wheat_igc.df$wheat_index_igc = NULL

## Get exchange rate data
top10currencies = c("EUR", "JPY", "GBP", "CAD", "MXN", "NZD")
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

## Create time
er.df$date = as.Date(rownames(er.df))

## Construct the final data frame
merged.df = na.omit(merge(wheat_igc.df, er.df, by = "date", all = FALSE))

## Create the lagged terms. We create the lagged terms after the
## mergee as the sampling frequency of the wheat price and the
## exchange rates are different.
lagNum = 260
for(i in exchange_rate_names){
    for(j in 1:lagNum){
        new_name = paste0(i, "_lag", j)
        merged.df[[new_name]] =
            c(rep(NA, j), merged.df[[i]][(1:(NROW(merged.df) - j))])
    }
}


final.df = na.omit(merged.df)

save(final.df, file = "final.Rdata")
