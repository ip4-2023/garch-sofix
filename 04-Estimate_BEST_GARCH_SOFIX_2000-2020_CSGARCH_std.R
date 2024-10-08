# Econometric Analysis of SOFIX Index with GARCH Models
# Paper at mdpi, Journal of Risk and Financial Management
#
# This script will estimate five comlete (with AR and MA components included in mean equation) garch models, with six different distributions,
# and save estimation results in a .html file

# OPTIONS
ar_lag <- 1 # lag used for ar term in mean equation 
ma_lag <- 1 # lag used for ma term in mean equation 
arch_lag <- 1 # lag in arch effect 
garch_lag <- 1 # lag in garch effect 
models_to_estimate <- c('csGARCH') # see rugarch manual for more
distribution_to_estimate <-  c( 'std') # distribution used in all models
my_html_file <- 'tabs/tab09_SOFIX_2000-2020.html' # where to save html file?

# END OPTIONS

library(tidyverse)
library(FinTS)
library(texreg)
library(rugarch)
library(readxl)

# close all opened windows
graphics.off()

# change working directory
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

# source functions
source('fcts/garch_fcts.R')

# get all combinations of models
df_grid <- expand_grid(ar_lag,
                       ma_lag,
                       arch_lag,
                       garch_lag,
                       models_to_estimate,
                       distribution_to_estimate)

# get price data

df_prices <- read_excel("data/SOFIX_2000-2020.xlsx", col_types = c("date", 
                                                          "text", "numeric", "numeric", "numeric", "numeric", 
                                                          "text"))

estimate_garch <- function(ar_lag,
                           ma_lag,
                           arch_lag,
                           garch_lag,
                           models_to_estimate,
                           distribution_to_estimate) {
  
  message('Estimating ARMA(',ar_lag,',', ma_lag, ')', '-',
          models_to_estimate, '(', arch_lag, ',', garch_lag, ') ', 
          'dist = ', distribution_to_estimate)
  
  # estimate model
  my_spec <- ugarchspec(variance.model = list(model = models_to_estimate,
                                              garchOrder = c(arch_lag, 
                                                             garch_lag)),
                        mean.model = list(armaOrder = c(ar_lag,
                                                        ma_lag)), 
                        distribution.model = distribution_to_estimate)
  
  my_garch_2000_2020 <- ugarchfit(spec = my_spec, data = df_prices$log_ret)
  
  return(my_garch_2000_2020)
}

# estimate all models
l_args <- as.list(df_grid)
l_models <- pmap(.l = l_args, .f = estimate_garch)

# make sure dir "tabs" exists
if (!dir.exists('tabs')) dir.create('tabs')

# reformat models for texreg
l_models <- map(l_models, extract.rugarch, include.rsquared = FALSE)

# write custom row
custom_row <- list('Variance Model' = df_grid$models_to_estimate,
                   'Distribution' = df_grid$distribution_to_estimate)
custom_names <- paste0('Model ', 1:length(l_models))

# save to html
htmlreg(l_models, 
        file = my_html_file, 
        custom.gof.rows = custom_row,
        custom.model.names = custom_names, 
        digits = 3)

# print to screen
screenreg(l_models,
          custom.gof.rows = custom_row,
          custom.model.names = custom_names, 
          digits = 3)


