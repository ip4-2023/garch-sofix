# Econometric Analysis of SOFIX Index with GARCH Models
# Paper at mdpi, Journal of Risk and Financial Management
#
# This script will estimate several garch models and find the best using the BIC
# criteria. A plot with the results, Figure 05 in the paper, is saved in a .png file
# at folder /figs. 

## MAIN OPTIONS

max_lag_AR <- 1 # used 1 in paper
max_lag_MA <- 1 # used 1 in paper
max_lag_ARCH <- 2 # used 2 in paper
max_lag_GARCH <- 1 # used 1 in paper
dist_to_use <-c('std') # see rugarch::ugarchspec help for more
models_to_estimate <- c('sGARCH', 'eGARCH', 'gjrGARCH',  'iGARCH', 'csGARCH') # see rugarch manual for more


## END OPTIONS

library(tidyverse)
library(purrr)

graphics.off()

my_d <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my_d)

source('fcts/garch_fcts.R')

# get price data
df_prices <- read_rds('data/SOFIX.rds')


out_std <- find_best_arch_model(x = df_prices$log_ret, 
                            type_models = models_to_estimate,
                            dist_to_use = dist_to_use,
                            max_lag_AR = max_lag_AR,
                            max_lag_MA = max_lag_MA,
                            max_lag_ARCH = max_lag_ARCH,
                            max_lag_GARCH = max_lag_GARCH)

# get table with estimation results

tab_out_std <- out$tab_out_std





# pivot table to long format (better for plotting)
df_long_std <- tidyr::pivot_longer(data = tab_out %>%
                                 select(model_name,
                                        type_model,
                                        type_dist,
                                        AIC, BIC),  cols = c('AIC', 'BIC'))

models_names <- unique(df_long_std$model_name)
best_models <- c(tab_out_std$model_name[which.min(tab_out_std$AIC)],
                 tab_out_std$model_name[which.min(tab_out_std$BIC)])

# figure out where is the best model
df_long_std <- df_long_std %>%
  mutate(order_model = if_else(model_name %in% best_models, 'Best Model', 'Not Best Model') ) %>%
  na.omit()

# make table with best models
df_best_models_std <- df_long_std %>%
  group_by(name) %>%
  summarise(model_name = model_name[which.min(value)],
            value = value[which.min(value)],
            type_model = type_model[which.min(value)])

# plot results
p1 <- ggplot(df_long_std %>%
               arrange(type_model), 
             aes(x = reorder(model_name, 
                             order(type_model)),
                 y = value, 
                 shape = type_dist,
                 color = type_model)) + 
  geom_point(size = 1.5, alpha = 0.65) + 
  coord_flip() + 
  theme_bw(base_family = "TT Times New Roman") + 
  facet_wrap(~name, scales = 'free_x') + 
  geom_point(data = df_best_models_std, mapping = aes(x = reorder(model_name, 
                                                              order(type_model)),
                                                  y = value), 
             color = 'blue', size = 5, shape = 8) +
  labs(title = 'Selecting Garch Models by Information Criteria', 
       subtitle = 'The best model is the one with lowest AIC or BIC (with star)',
       x = 'Models for testing',
       y = 'Value of Information Criteria',
       shape = 'Type of Dist.',
       color = 'Type of Model') + 
  theme(legend.position = "right")

x11()  ; p1 ; ggsave('figs/fig05_best_garch_std-5X6.png', limitsize = TRUE)

# estimate best garch model by BIC (used in next section)
best_spec_std = ugarchspec(variance.model = list(model =  out$best_bic$type_model, 
                                             garchOrder = c(out$best_bic$lag_arch,
                                                            out$best_bic$lag_garch)),
                       mean.model = list(armaOrder = c(out$best_bic$lag_ar, 
                                                       out$best_bic$lag_ma)),
                       distribution = 'std')

my_best_garch_std <- ugarchfit(spec = best_spec, 
                           data = df_prices$log_ret)


write_rds(my_best_garch_std, 'data/garch_model5X6_std.rds')


View(df_long_std)