library(rWBclimate)
countryLocator = c(NoAm_country, SoAm_country, Oceana_country, Africa_country, 
                   Asia_country, Eur_country)

test = get_historical_precip(locator = countryLocator, time_scale = "month")


library(raincpc)
cpc_get_rawdata(2016, 01, 01, 2016, 01, 01)
test = cpc_read_rawdata(2016, 01, 01)

Fn_Flip_Matrix_Rows <- function(mat) {
    return(mat[nrow(mat):1, ])
}
# function to rotate a matrix 90 degress clockwise for plotting only used to
# counteract the 'image' function default behavior
Fn_Rotate_Matrix <- function(mat) {
    return(t(mat)[, nrow(mat):1])
}

test2 = Fn_Rotate_Matrix(as.matrix(test))

library(ggplot2)
test.df = data.frame(x = rep(1:720, 360), y = rep(1:360, each = 720), rain = c(test2))

ggplot(data = test.df, aes(x = x, y = y, fill = log(rain + 1))) +
    geom_tile()


## Only about 36% of the rainfall matrix contains data
var = 100
obs = 300
x = matrix(rnorm(var * obs), ncol = var)
predTime = (obs * 0.75 + 1):(obs)
set.seed(587)
alpha = rnorm(var)
y = c(x %*% alpha)

library(neuralnet)
x.df = data.frame(cbind(y, x))
train.df = x.df[1:(obs * 0.75), ]
pred.df = x.df[(obs * 0.75 + 1):obs, ]
nn.formula = as.formula(paste0("y ~ ", paste0(colnames(x.df)[-1], collapse = " + ")))
test.nn = neuralnet(nn.formula, hidden = 5, data = train.df)

library(caret)
control.nn = trainControl(number = 3)
grid.nn = expand.grid(layer1 = c(5, 10, 15, 20), layer2 = 0, layer3 = 0)
system.time(
{
    test.fit = train(nn.formula, data = train.df, method = "neuralnet", tuneGrid = grid.nn,
                     trControl = control.nn)
})



plot(pred.df$y, type = "l")
pred.nn = compute(test.nn, pred.df[, -1])$net.result
lines(pred.nn, col = "red")
pred.nn2 = predict(test.fit, pred.df[, -1])
lines(pred.nn2, col = "steelblue")

## Check the mse
mse = function(x, y){
    sum((x - y)^2)
}
mse(pred.df$y, pred.nn)
mse(pred.df$y, pred.nn2)


## The models are the same, but the prediction is different. Check the
## difference between the compute and the predict function.



library(forecast)
n = 300
maxLag = 5
predTime = (n * 0.75 + 1):(n)
test.df =
    data.frame(sim = c(arima.sim(model = list(ar = c(0.6, 0.3, 0.05)), n = n)))
shift = function(x, lag){
    n = length(x)
    c(rep(NA, lag), x[1:(n - lag)])
}
for(i in 1:maxLag){
    test.df[[paste0("lag", i)]] = shift(test.df$sim, i)
}

train.df = test.df[(maxLag + 1):(n * 0.75), ]
pred.df = test.df[(n * 0.75 + 1):n, ]


(baseModel = auto.arima(test.df$sim[1:(n * 0.75)]))

nn.formula = as.formula(paste0("sim ~ ", paste0(colnames(train.df)[-1], collapse = " + ")))
control.nn = trainControl(number = 3)
grid.nn = expand.grid(layer1 = c(5, 10, 15, 20), layer2 = 0, layer3 = 0)
system.time(
{
    nnModel = train(nn.formula, data = train.df, method = "neuralnet", tuneGrid = grid.nn,
                    trControl = control.nn)
})

plot(test.df$sim, type = "l")
## lines(test.df$sim[1:(n * 0.75)], col = "red", lty = 2)
basePred = predict(baseModel, n.ahead = n * 0.25)$pred
lines(basePred, col = "red", lty = 2)
nnPred = predict(nnModel, newdata = pred.df[, -1])
lines(predTime, nnPred, col = "steelblue", lty = 2)

mse(test.df$sim[predTime], basePred)
mse(test.df$sim[predTime], nnPred)


## Test neural network with production
library(FAOSTAT)

cerealQuery = data.frame(elementCode = c(5510, 5510, 5510),
                         itemCode = c("15", "27", "56"),
                         domainCode = c("QC", "QC", "QC"))
cereals = getFAOtoSYB(query = cerealQuery)
wfbs = cereals$aggregates[cereals$aggregates$FAOST_CODE == 5000, ]
## colnames(wfbs)[colnames(wfbs) == "QC_1717_5510"] = "total_cereal_prod"
colnames(wfbs)[colnames(wfbs) %in% c("QC_15_5510", "QC_27_5510", "QC_56_5510")] = c("total_wheat_prod", "total_rice_prod", "total_maize_prod")


n = NROW(wfbs)
maxLag = 5
predTime = (n * 0.75 + 1):(n)

test.df =
    data.frame(sim = wfbs$total_wheat_prod)
shift = function(x, lag){
    n = length(x)
    c(rep(NA, lag), x[1:(n - lag)])
}
for(i in 1:maxLag){
    test.df[[paste0("lag", i)]] = shift(test.df$sim, i)
}

train.df = test.df[(maxLag + 1):(n * 0.75), ]
pred.df = test.df[(n * 0.75 + 1):n, ]


(baseModel = auto.arima(test.df$sim[1:(n * 0.75)]))

nn.formula = as.formula(paste0("sim ~ ", paste0(colnames(train.df)[-1], collapse = " + ")))
control.nn = trainControl(number = 3)
grid.nn = expand.grid(layer1 = c(1:5), layer2 = 0, layer3 = 0)
system.time(
{
    nnModel = train(nn.formula, data = train.df, method = "neuralnet", tuneGrid = grid.nn,
                    trControl = control.nn)
})

plot(test.df$sim, type = "l")
## lines(test.df$sim[1:(n * 0.75)], col = "red", lty = 2)
basePred = predict(baseModel, n.ahead = n * 0.25)$pred
lines(basePred, col = "red", lty = 2)
nnPred = predict(nnModel, newdata = pred.df[, -1])
lines(predTime, nnPred, col = "steelblue", lty = 2)

mse(test.df$sim[predTime], basePred)
mse(test.df$sim[predTime], nnPred)
