# Econometric Analysis of SOFIX Index with GARCH Models 
# Paper at mdpi, Journal of Risk and Financial Management
# The resulting dataset is serialized (saved) in a rds file named data/SOFIX_2007-2024.rds,
# to be used in the next step.

## MAIN OPTIONS (fell free to edit it)

first_date <- '2007-01-03' # first date in sample 
last_date <- '2024-03-28' # set Sys.Date() for current date 
my_ticker <- '^SOFIX' 
series_name <- 'SOFIX' # Name of index/stock that will show up in all plots

## END OPTIONS

# load required libraries
library(BatchGetSymbols)
library(tidyverse)

# change directory to where the script located
my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

# makes sure the directory "data" exists
if (!dir.exists('data')) dir.create('data')

# download price data 



library(readxl)
df_prices  <- read_excel("data/SOFIX_2007-2024.xlsx", col_types = c("date", 
                                                     "text", "numeric", "numeric", "numeric", "numeric", 
                                                     "text"))




# save data into file
rds_out <- 'data/SOFIX_2007-2024.rds'
write_rds(df_prices, rds_out)


