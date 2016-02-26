########################################################################
## This script examines the correlation relationship betwee the wheat
## price index and the exchange rates
########################################################################

load("final.Rdata")

## Examine the ACF and the PACF
acf(final.df$log_wheat_index_igc, lag.max = 1000)
pacf(final.df$log_wheat_index_igc, lag.max = 10)
acf(diff(final.df$log_wheat_index_igc), lag.max = 100)
pacf(diff(final.df$log_wheat_index_igc), lag.max = 10)

## Examine the CCF
par(mfrow = c(7, 3), mar = c(0, 4, 0, 0))
for(i in grep("USD\\.[A-Z]+$", colnames(final.df), value = TRUE)){
    ccf(diff(final.df[, "log_wheat_index_igc"]), diff(final.df[, i]),
        ylab = i)
}

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
exchange_rate_names = grep("^USD.[A-Z]+$", colnames(final.df), value = TRUE)
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


