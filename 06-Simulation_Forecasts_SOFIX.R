# Econometric Analysis of SOFIX Index with GARCH Models 
# Paper at mdpi, Journal of Risk and Financial Management
# 
# This script will use the best garch model from previous script and simulate
# many return series into the future. After the simulations, the code calculates 
# probabilities for the simulated paths to reach the maximum value of index SOFIX.

## OPTIONS

set.seed(20200315) # fix seed for simulations (20200315 replicates the paper's results)
n_sim <- 5000 # number of simulations (5000 was used in paper,
# be aware that this code is memory intensive and might freeze your computer. 
# Increase n_sim at your own risk!!
n_days_ahead <- 75*365 # Number of days ahead to simulate (10*365 in paper)

## END OPTIONS

library(tidyverse)
library(ggtext)
library(lubridate)
library(writexl)
library(expss)
library(openxlsx)


graphics.off()

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

# get price and model data
df_prices <- read_rds('data/SOFIX.rds')

df_prices$ref.date <- as.Date(df_prices$ref.date, format = "%Y-%m-%d")

my_garch <- read_rds('data/garch_model5X6.rds')


series_name <- df_prices$series_name[1]


# source functions
source('fcts/garch_fcts.R')



# do simulations
df_sim <- do_sim(n_sim = n_sim, 
                 n_t = n_days_ahead, 
                 my_garch, 
                 df_prices = df_prices)

# calculate probabilities of reaching peak value
tab_prob <- df_sim %>%
  group_by(ref_date) %>%
  summarise(prob = mean(sim_price > max(df_prices$price.adjusted)))



n_years_back <- 25
df_prices_temp <- df_prices %>%
  dplyr::filter(ref.date > max(ref.date) - n_years_back*365)

my_garch_name <- toupper(as.character(my_garch@model$modeldesc$vmodel))



p1 <- ggplot() + 
  geom_line(data = df_prices_temp, 
            aes(x = ref.date, y = price.adjusted), color = 'black', size = 0.75)  + 
  geom_line(data = df_sim, 
            aes(x = ref_date, 
                y = sim_price, 
                group = i_sim),
            color = 'grey', 
            size = 0.25,
            alpha = 0.015) + 
  theme_bw(base_family = "Times New Roman") + 
  geom_hline(yintercept = max(df_prices_temp$price.adjusted)) + 
  labs(title = paste0('Price Projections of ', series_name),
       subtitle = paste0('Total of ', n_sim, ' simulations based on a ',
                         my_garch_name, 
                         ' model selected by BIC'),
       caption = 'Data from https://stooq.com',
       x = 'Years',
       y = 'Value') + 
  ylim(c(0.75*min(df_prices_temp$price.adjusted), 
         1.25*max(df_prices_temp$price.adjusted))) + 
  xlim(c(max(df_prices_temp$ref.date) - n_years_back*365,
         max(df_prices_temp$ref.date) + 5*365) )


# plot graphics
x11(); p1 ; ggsave(paste0('figs/fig04a', series_name, '_price_simulation.png'))



my_idx_date <- first(which(tab_prob$prob > 0.5))
df_date <- tibble(idx = c(first(which(tab_prob$prob > 0.001)),
                          first(which(tab_prob$prob > 0.5)),
                          first(which(tab_prob$prob > 0.75)),
                          first(which(tab_prob$prob > 0.90))),
                  ref_date = tab_prob$ref_date[idx],
                  prob = tab_prob$prob[idx],
                  my_text = paste0(format(ref_date, '%m/%d/%Y'),
                                   '\nprob = ', scales::percent(prob) ) )

df_textbox <- tibble(ref_date = df_date$ref_date[2],
                     prob = 0.25,
                     label = paste0('According to the estimated _', my_garch_name, '_ model, ', 
                                    'the chances of asset **', series_name, '** to reach ',
                                    'its historical peak value of ', 
                                    format(max(df_prices$price.adjusted), 
                                           big.mark = ',',
                                           decimal.mark = '.'),
                                    ' are higher than 50% at ', format(ref_date, '%m/%d/%Y'), '.') )

p2 <- ggplot(tab_prob, aes(x = ref_date, y = prob) ) + 
  geom_line(size = 2) + 
  labs(title = paste0('Probabilities of ', series_name, ' Reaching its Historical Peak'),
       subtitle = paste0('Calculations based on simulations of ',
                         my_garch_name, 
                         ' model'),
       x = 'Years',
       y = 'Probability') + 
  scale_y_continuous(labels = scales::percent) + 
  geom_point(data = df_date,
             aes(x = ref_date, y = prob), size = 5, color = 'red') + 
  geom_text(data = df_date, aes(x = ref_date, y = prob, 
                                label = my_text), 
            nudge_x = nrow(tab_prob)*0.085,
            nudge_y = -0.05,
            color ='red', check_overlap = TRUE) + 
  geom_textbox(data = df_textbox, 
               mapping = aes(x = ref_date, 
                             y = prob, 
                             label = label),
               width = unit(0.4, "npc"),
               #fill = "cornsilk",
               hjust = 0) + 
  theme_bw(base_family = "Times New Roman")

x11(); p2 ; ggsave(paste0('figs/fig04b', series_name, '_prob_reaching_peak.png'))



