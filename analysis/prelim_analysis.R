########################################################################
## Title: Commodity price and foreign exchange rate
## Date: 2016-01-04
########################################################################

## Prepare the data for the FFPI
ffpiUrl = "http://www.fao.org/fileadmin/templates/worldfood/Reports_and_docs/Food_price_indices_data.csv"
ffpi = read.csv(file = ffpiUrl, skip = 2)
ffpi$Date = as.character(ffpi$Date)
ffpi$Month = as.numeric(sapply(strsplit(ffpi$Date, "/"), FUN = function(x) x[1]))
ffpi$Year = as.numeric(sapply(strsplit(ffpi$Date, "/"), FUN = function(x) x[2]))
ffpi$Date = NULL
redudantColumns = grep("^X", colnames(ffpi))
finalffpi = ffpi[-redudantColumns]

## Scrap usd exchange rate index
library(rvest)
usdFxUrl = "http://www.federalreserve.gov/releases/h10/summary/indexn_m.htm"

usdFx = 
    usdFxUrl %>%
    read_html() %>%
    html_nodes(xpath = '//*[@id="printThis"]/table') %>%
    html_table() %>%
    .[[1]]



usdFx$MonthName = sapply(strsplit(usdFx$X1, " "), FUN = function(x) x[1])
usdFx$Year = as.numeric(sapply(strsplit(usdFx$X1, " "), FUN = function(x) x[2]))
usdFx$Month = match(usdFx$MonthName, toupper(month.abb))
usdFx$MonthName = NULL
usdFx$X1 = NULL
colnames(usdFx)[which(colnames(usdFx) == "X2")] = "usdFx"

## Merge the two datasets
all.df = merge(finalffpi, usdFx, by = c("Month", "Year"), all = FALSE)
sorted.df = all.df[with(all.df, order(Year, Month)), ]

## Correlation
##
## NOTE (Michael): It might be better off to predict each of the
##                 commodity index, rather than the aggregated
##                 fpi. This way, the effect can be isolated.
##
## NOTE (Michael): There seems to be very strong correlation between
##                 cereals and oil. Further, their correlation with
##                 the US exchange rate is very similar. This raise
##                 the question whether they are competing assets
##                 driven by the same factor.
cor(sorted.df)
pairs(~., data = sorted.df)


## Exploratory plots
par(mfrow = c(3, 1))
with(sorted.df,
{
    plot(Food.Price.Index, type = "l", ylim = c(0, 300))
    lines(usdFx, col = "red")
    plot(Food.Price.Index, usdFx)
    abline(lm(usdFx ~ Food.Price.Index), col = "red", lty = 2)
    ccf(Food.Price.Index, usdFx)
})

## It would appear that the us exchange rate has a 1 month lead ahead
## of the food price index, the correlation is negative.
with(sorted.df, {
     n = NROW(sorted.df)
     summary(lm(Food.Price.Index[-1] ~ usdFx[-n]))
})


with(sorted.df,{
     n = NROW(sorted.df)
     cc = ccf(Food.Price.Index, usdFx, plot = FALSE)
     print(cc)
     cc$acf[which.max(abs(cc$acf))]
})

library(forecast)
n = NROW(sorted.df)
pred.n = 12
train.df = sorted.df[1:(n - pred.n), ]
pred.df = sorted.df[(n - pred.n + 1):n, ]


(baseModel = with(train.df, auto.arima(Food.Price.Index)))
(fxModel = with(train.df, auto.arima(Food.Price.Index, xreg = data.frame(usdFx))))
with(sorted.df, {
    plot(Food.Price.Index, type = "l")
    lines(fitted(baseModel), col = "red")
    lines(predict(baseModel, n.ahead = pred.n)$pred, col = "blue")
    lines(predict(fxModel, newxreg = data.frame(pred.df$usdFx), n.ahead = pred.n)$pred, col = "green")
})


########################################################################
## Lets just focus on the cereals first
########################################################################
library(FAOSTAT)
library(forecast)
## cerealQuery = data.frame(elementCode = c(5510),
##                          itemCode = c("1717"),
##                          domainCode = c("QC"))

cerealQuery = data.frame(elementCode = c(5510, 5510, 5510),
                         itemCode = c("15", "27", "56"),
                         domainCode = c("QC", "QC", "QC"))
cereals = getFAOtoSYB(query = cerealQuery)
pop = getWDI()
wpop = pop[pop$Country == "World", c("Year", "SP.POP.TOTL")]
wfbs = cereals$aggregates[cereals$aggregates$FAOST_CODE == 5000, ]
## colnames(wfbs)[colnames(wfbs) == "QC_1717_5510"] = "total_cereal_prod"
colnames(wfbs)[colnames(wfbs) %in% c("QC_15_5510", "QC_27_5510", "QC_56_5510")] = c("total_wheat_prod", "total_rice_prod", "total_maize_prod")
cerealsAnalysis.df =
    merge(sorted.df, wfbs[, c("Year", grep("prod", colnames(wfbs), value = TRUE))],
          by = "Year", all = FALSE)
## cerealsAnalysis.df$smooth_cereal_prod =
##     with(cerealsAnalysis.df, spline(Year, total_cereal_prod, n = NROW(cerealsAnalysis.df))$y/10000)
cerealsFinal.df = merge(cerealsAnalysis.df, wpop, by = "Year", all = FALSE)
cerealsFinal.df$SP.POP.TOTL = cerealsFinal.df$SP.POP.TOTL/10000
## cerealsFinal.df$pcap = with(cerealsFinal.df, smooth_cereal_prod/SP.POP.TOTL)
cerealsFinal.df[, grep("prod", colnames(wfbs), value = TRUE)] =
    cerealsFinal.df[, grep("prod", colnames(wfbs), value = TRUE)]/10000


## Get some climate data for the forecast of the production
library(rWBclimate)

## bu.df = cerealsFinal.df
## cerealsFinal.df = cerealsFinal.df[180:300, ]

n = NROW(cerealsFinal.df)
pred.n = 36
train.df = cerealsFinal.df[1:(n - pred.n), ]
pred.df = cerealsFinal.df[(n - pred.n + 1):n, ]

## par(mfrow = c(2, 1))
(baseModel = with(train.df, auto.arima(Cereals.Price.Index)))
(fxModel = with(train.df, auto.arima(Cereals.Price.Index, xreg = data.frame(usdFx))))
(prodModel =
    with(train.df,
         auto.arima(Cereals.Price.Index,
                    xreg = train.df[, grep("prod", colnames(train.df), value = TRUE)])))
## (supModel = with(train.df, auto.arima(Cereals.Price.Index, xreg = data.frame(pcap))))
(fullModel =
    with(train.df,
         auto.arima(Cereals.Price.Index,
                    xreg = train.df[, c("usdFx", "SP.POP.TOTL",
                                        grep("prod", colnames(train.df), value = TRUE))])))


## 1990 ~ 2014
with(cerealsFinal.df, {
    plot(Cereals.Price.Index, type = "l", ylim = c(0, 300))
    lines(fitted(baseModel), col = "red")
    lines(predict(baseModel, n.ahead = pred.n)$pred, col = "blue")
    lines(predict(fxModel, newxreg = data.frame(pred.df$usdFx), n.ahead = pred.n)$pred,
          col = "green")
    lines(predict(prodModel,
                  newxreg = pred.df[, grep("prod", colnames(pred.df), value = TRUE)],
                  n.ahead = pred.n)$pred,
          col = "gold")
    ## lines(predict(supModel, newxreg = data.frame(pred.df$pcap), n.ahead = pred.n)$pred,
    ##       col = "red")
    lines(predict(fullModel,
                  n.ahead = pred.n,
                  newxreg = pred.df[,
                                    c("usdFx", "SP.POP.TOTL",
                                      grep("prod", colnames(pred.df), value = TRUE))])$pred,
          col = "steelblue")
})


with(cerealsFinal.df,
     plot(Cereals.Price.Index, pcap))


library(splines)
## Create smoother for production
with(cerealsFinal.df,{
    plot(Year, total_cereal_prod, type = "l")
    lFit = loess(total_cereal_prod ~ Year, span = 20)
    ## print(str(lFit))
    lines(seq(from = min(cerealsFinal.df$Year),
              to = max(cerealsFinal.df$Year),
              length.out = length(lFit$fitted)),
          lFit$fitted, col = "blue")
}
)

with(cerealsFinal.df,{
    plot(Year, total_cereal_prod, type = "l")
    sFit = spline(Year, total_cereal_prod, n = 300)
    ## print(str(lFit))
    lines(sFit)
    ## lines(seq(from = min(cerealsFinal.df$Year),
    ##           to = max(cerealsFinal.df$Year),
    ##           length.out = length(lFit$fitted)),
    ##       sFit, col = "blue")
}
)
