## # Econometric Analysis of SOFIX Index with GARCH Models
# Paper at mdpi, Journal of Risk and Financial Management
#
#

library(rugarch)
library(tidyverse)
library(ggtext)
library(lubridate)
library(writexl)
library(expss)
library(openxlsx)
library(dplyr)
library(quantmod)
library(caTools)
    library(dplyr)
    library(ggplot2)
    library(lubridate)
    library(gridExtra)
    library(forecast)
    library(modeltime)
    library(fabletools)
    library(feasts)
    library(tsibble)
    library(tibble)
    library(fable)
    library(data.table) 
    library(knitr) 


graphics.off()

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

# get price and model data
df_prices <- read_rds('data/SOFIX.rds')


df_prices$ref.date <- as.Date(df_prices$ref.date, format = "%Y-%m-%d")

my_garch <- read_rds('data/garch_model5X6.rds')


series_name <- df_prices$series_name[1]



library(xts)
df_prices <- as.xts(df_prices)
train_date <- nrow(df_prices) *0.861592
train <- df_prices[1:train_date,]

test <- df_prices[-c(1:train_date),]

dim(train)
dim(test)
view(train)
view(test$log_ret)

# source functions
source('fcts/garch_fcts.R')

# do forecasts

# sGARCH(1,1) + MU + ARMA(1,1)
spec = ugarchspec(
  variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE), 
  distribution.model = "std")

sgarch.fit1 = ugarchfit(spec = spec, data=df_prices[,"log_ret", drop = FALSE], 
                        out.sample = 800, solver = "solnp")

print(sgarch.fit1)
plot(sgarch.fit1, which=3)

sgarch.pred1 = ugarchforecast(sgarch.fit1, n.ahead = 800)
plot(sgarch.pred1, which=3)


# eGARCH(1,1) + MU + ARMA(1,1)
spec = ugarchspec(
  variance.model = list(model = "eGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE), 
  distribution.model = "std")

egarch.fit2 = ugarchfit(spec = spec, data=df_prices[,"log_ret", drop = FALSE], 
                          out.sample = 800, solver = "solnp")
print(egarch.fit2)
plot(egarch.fit2, which=3)

egarch.pred2 = ugarchforecast(egarch.fit2, n.ahead = 800)
plot(egarch.pred2, which=3)


# gjrGARCH(1,1) + MU + ARMA(1,1)
spec = ugarchspec(
  variance.model = list(model = "gjrGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE), 
  distribution.model = "std")

gjrgarch.fit3 = ugarchfit(spec = spec, data=df_prices[,"log_ret", drop = FALSE], 
                          out.sample = 800, solver = "solnp")

print(gjrgarch.fit3)
plot(gjrgarch.fit3, which=3)

gjrgarch.pred3 = ugarchforecast(gjrgarch.fit3, n.ahead = 800)
plot(gjrgarch.pred3, which=3)


# iGARCH(1,1) + MU + ARMA(1,1)

spec = ugarchspec(
  variance.model = list(model = "iGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE), 
  distribution.model = "std")

igarch.fit4 = ugarchfit(spec = spec, data=df_prices[,"log_ret", drop = FALSE], 
                        out.sample = 800, solver = "solnp")

print(igarch.fit4)
plot(igarch.fit4, which=3)

igarch.pred4 = ugarchforecast(igarch.fit4, n.ahead = 800)
plot(igarch.pred4, which=3)

# csGARCH(1,1) + MU + ARMA(1,1)

spec = ugarchspec(
  variance.model = list(model = "csGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE), 
  distribution.model = "std")

csgarch.fit5 = ugarchfit(spec = spec, data=df_prices[,"log_ret", drop = FALSE], 
                        out.sample = 800, solver = "solnp")
print(csgarch.fit5)
plot(csgarch.fit5, which=3)

csgarch.pred5 = ugarchforecast(csgarch.fit5, n.ahead = 800)
plot(csgarch.pred5, which=1)
plot(csgarch.pred5, which=3)



#========

#cat("\nrugarch-->test4-4: Forecast Performance Measures Test (GARCH)\n")
tic = Sys.time()

# fpm tests

options(width=150)
zz <- file("test-forecasts_performance.txt", open="wt")
sink(zz)
print(fpm(sgarch.pred1, summary = TRUE))
print(fpm(sgarch.pred1, summary = FALSE))
print(fpm(egarch.pred2, summary = TRUE))
print(fpm(egarch.pred2, summary = FALSE))
print(fpm(gjrgarch.pred3, summary = TRUE))
print(fpm(gjrgarch.pred3, summary = FALSE))
print(fpm(igarch.pred4, summary = TRUE))
print(fpm(igarch.pred4, summary = FALSE))
print(fpm(csgarch.pred5, summary = TRUE))
print(fpm(csgarch.pred5, summary = FALSE))
sink(type="message")
sink()
close(zz)


pred.list = list(garch=sgarch.pred1,
                 egarch=egarch.pred2,
                 gjrgarch=gjrgarch.pred3,
                 igarch=igarch.pred4,
                 cgarch=csgarch.pred5)
fpm.mat = sapply(pred.list, fpm)
print(fpm.mat)




