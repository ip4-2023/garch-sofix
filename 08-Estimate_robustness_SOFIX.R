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

View(df_prices)

library(xts)
df_prices <- as.xts(df_prices)
train_date <- nrow(df_prices) *0.861592
train <- df_prices[1:train_date,]

test <- df_prices[-c(1:train_date),]


# source functions
source('fcts/garch_fcts.R')

# do forecasts



# csGARCH(1,1) + MU + ARMA(1,1), 2000-2020

spec = ugarchspec(
  variance.model = list(model = "csGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE), 
  distribution.model = "std")

csgarch.fit_1 = ugarchfit(spec = spec, data=df_prices[,"log_ret", drop = FALSE], 
                        out.sample = 800, solver = "solnp")
print(csgarch.fit_1)

#=========
# csGARCH(1,1) + MU + ARMA(1,1), full range

spec = ugarchspec(
  variance.model = list(model = "csGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE), 
  distribution.model = "std")

csgarch.fit_2 = ugarchfit(spec = spec, data=df_prices[,"log_ret", drop = FALSE], 
                         solver = "solnp")
print(csgarch.fit_2)


# csGARCH(1,1) + MU + ARMA(1,1), 2007-2024
library(xts)
df_prices <- as.xts(df_prices)
train_date <- nrow(df_prices) *0.26349481
train <- df_prices[1:train_date,]
test <- df_prices[-c(1:train_date),]


spec = ugarchspec(
  variance.model = list(model = "csGARCH", garchOrder = c(1,1)), 
  mean.model = list(armaOrder = c(1,1), include.mean = TRUE), 
  distribution.model = "std")

csgarch.fit_3 = ugarchfit(spec = spec, data=test[,"log_ret", drop = FALSE], 
                         solver = "solnp")
print(csgarch.fit_3)

