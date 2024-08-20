# Econometric Analysis of SOFIX Index with GARCH Models 
# Paper at mdpi, Journal of Risk and Financial Management

# This script will test for arch effects for a given vector of returns, given
# lags and save results in .html file.

# This script will also test for unit roots in log return. 

## OPTIONS

max_lag <- 5
my_html_file <- 'tabs/tab04-Arch_Test-sofix.html'
my_xlsx_file <- 'tabs/tab04-Arch_Test-sofix.xlsx'

## END OPTIONS

library(tidyverse)
library(knitr)
library(kableExtra)
library(writexl)
library(urca)


my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

source('fcts/garch_fcts.R')

# create directory
if (!dir.exists(dirname(my_html_file))) dir.create(dirname(my_html_file))

# get price data
df_prices <- read_rds('data/SOFIX.rds')

# do arch test
tab_out <- do_arch_test(x = df_prices$log_ret, max_lag = max_lag)

tab_out

# remove attributes of table so it can be correctly parsed in html
tab_out <- as.data.frame(
  lapply(tab_out, function(x) { attributes(x) <- NULL; x })
)
str(tab_out)

rownames(tab_out) <- NULL

# save table in html
my_tbl <- knitr::kable(tab_out, format = 'html' ) %>%
  kable_styling(bootstrap_options = c("striped"), 
                full_width = FALSE ) 

my_tbl

cat(my_tbl, file = my_html_file)  

# write to excel
write_xlsx(x = tab_out, path = my_xlsx_file)

# UR tests

## ADF tests levels
ADF_NC = ur.df(df_prices$log_ret, type= 'none', selectlags =  'AIC')
summary(ADF_NC)

ADF_C = ur.df(df_prices$log_ret, type='drif', selectlags =  'AIC')
summary(ADF_C)

ADF_CT = ur.df(df_prices$log_ret, type='trend',selectlags =  'AIC')
summary(ADF_CT)

## ADF tests first diff
ADF_NC_FD = ur.df(diff(df_prices$log_ret), type= 'none', selectlags =  'AIC')
summary(ADF_NC_FD)

ADF_C_FD = ur.df(diff(df_prices$log_ret), type='drif', selectlags =  'AIC')
summary(ADF_C_FD)

ADF_CT_FD = ur.df(diff(df_prices$log_ret), type='trend',selectlags =  'AIC')
summary(ADF_CT_FD)

## PP tests levels
PP_C <- ur.pp(df_prices$log_ret, type="Z-tau", model="constant", lags="long")
summary(PP_C)

PP_CT <- ur.pp(df_prices$log_ret, type="Z-tau", model="trend", lags="long")
summary(PP_CT)

## PP tests first diff
PP_C_FD <- ur.pp(diff(df_prices$log_ret), type="Z-tau", model="constant", lags="long")
summary(PP_C_FD)

PP_CT_FD <- ur.pp(diff(df_prices$log_ret), type="Z-tau", model="trend", lags="long")
summary(PP_CT_FD)


## KPSS tests levels

kpss_C_L <- ur.kpss(df_prices$log_ret, type="mu", lags="long")
summary(kpss_C_L)

kpss_CT_L <- ur.kpss(df_prices$log_ret, type="tau", lags="long")
summary(kpss_CT_L)


## KPSS first differences
kpss_C_L_FD <- ur.kpss(diff(df_prices$log_ret), type="mu", lags="long")
summary(kpss_C_L_FD)


kpss_CT_L_FD <- ur.kpss(diff(df_prices$log_ret), type="tau", lags="long")
summary(kpss_CT_L_FD)


## ERS tests levels
ers_C_L <- ur.ers(df_prices$log_ret, type="DF-GLS", model="const", lag.max=4)
summary(ers_C_L)

ers_CT_L <- ur.ers(df_prices$log_ret, type="DF-GLS", model="trend", lag.max=4)
summary(ers_CT_L)

## ERS first differences
ers_C_L_FD <- ur.ers(diff(df_prices$log_ret), type="DF-GLS", model="const", lag.max=4)
summary(ers_C_L_FD)


ers_CT_L_FD <- ur.ers(diff(df_prices$log_ret), type="DF-GLS", model="trend", lag.max=4)
summary(ers_CT_L_FD)

