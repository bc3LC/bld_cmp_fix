library(dplyr)
library(gcamdata)
library(tidyr)
library(ggplot2)

# Load data
# First read pop to scalate to per capita values
flsp <- read.csv("input/flsp.csv", skip = 1) %>%
  filter(grepl("resid", nodeInput)) %>%
  rename(flsp = base.building.size) %>%
  select(GCAM_region_ID, year, flsp) %>%
  complete(nesting(GCAM_region_ID), year = 1975:2015) %>%
  group_by(GCAM_region_ID) %>%
  mutate(flsp = if_else(is.na(flsp), approx_fun(year, flsp, rule = 1), flsp)) %>%
  ungroup()


serv_coal <- read.csv("input/serv_coal.csv") %>%
  filter(year >= 1975) %>%
  filter(en_EJ != 0) %>%
  as_tibble() %>%
  left_join_error_no_match(flsp, by = join_by(GCAM_region_ID, year)) %>%
  # Transform to GJ
  mutate(en_EJ_flsp = en_EJ / flsp) %>%
  mutate(log_en_EJ_flsp = log(en_EJ_flsp),
         log_pcgdp_thous = log(pcgdp_thous),
         log_price = log(price),
         log_sq_pcgdp_thous = log_pcgdp_thous^2,
         GCAM_region_ID = as.character(GCAM_region_ID)) %>%
  filter(en_EJ_flsp != 0,
         price != 0)%>%
  filter(complete.cases(.))

coal_heat <- serv_coal %>% filter(grepl("heating", service))
coal_oth <- serv_coal %>% filter(grepl("other", service))



serv_TradBio <- read.csv("input/serv_TradBio.csv") %>%
  filter(year >= 1975) %>%
  as_tibble() %>%
  left_join_error_no_match(flsp, by = join_by(GCAM_region_ID, year)) %>%
  mutate(en_EJ_flsp = en_EJ / flsp) %>%
  mutate(log_en_EJ_flsp = log(en_EJ_flsp),
         log_pcgdp_thous = log(pcgdp_thous),
         log_price = log(price),
         log_sq_pcgdp_thous = log_pcgdp_thous^2,
         GCAM_region_ID = as.character(GCAM_region_ID)) %>%
  filter(en_EJ_flsp != 0,
         price != 0) %>%
  filter(complete.cases(.))

tradBio_heat <- serv_TradBio %>% filter(grepl("heating", service))
tradBio_oth <- serv_TradBio %>% filter(grepl("other", service))
  
#--------------
# 1- TRADITIONAL BIOMASS
# Fit function for traditional biomass:
# Given its carbon neutrality and that the price is exogenous (not linked with the land module, we can drop the price effect out)
#formula.tradBio<- "en_EJ_flsp ~ a * pcgdp_thous^-b * price^-c"
#formula.tradBio<- "log(en_EJ_flsp) ~ a + log(pcgdp_thous) + log(pcgdp_thous^2) +log(price) + GCAM_region_ID"

prelast_tradBio <- -0.4

# Use minipack.lm for the estimation (need to check the prediction)
fit_tradBio_heat = lm(log_en_EJ_flsp ~ log_pcgdp_thous + log_sq_pcgdp_thous + GCAM_region_ID,
                        data = tradBio_heat)

fit_tradBio_oth = lm(log_en_EJ_flsp ~ log_pcgdp_thous + log_sq_pcgdp_thous + GCAM_region_ID,
                      data = tradBio_oth)

# Check coefficients
summary(fit_tradBio_heat)
summary(fit_tradBio_oth)

# Predict
data_tradBio <- serv_TradBio %>%
  mutate(b1_heat = as.numeric(fit_tradBio_heat$coefficients[1]),
         b2_heat = as.numeric(fit_tradBio_heat$coefficients[2]),
         b3_heat = as.numeric(fit_tradBio_heat$coefficients[3]),
         b1_oth = as.numeric(fit_tradBio_oth$coefficients[1]),
         b2_oth = as.numeric(fit_tradBio_oth$coefficients[2]),
         b3_oth = as.numeric(fit_tradBio_oth$coefficients[3]),
         prelast = prelast_tradBio) %>%
  mutate(pred_log_en_EJ_flsp = if_else(grepl("heat", service), 
                                       b1_heat + b2_heat * log_pcgdp_thous + b3_heat * log_sq_pcgdp_thous + prelast_tradBio * log_price,
                                       b1_oth + b2_oth * log_pcgdp_thous + b3_oth * log_sq_pcgdp_thous + prelast_tradBio * log_price))

data_tradBio_adder <- data_tradBio %>%
  group_by(GCAM_region_ID) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(en_EJ_flsp = exp(log_en_EJ_flsp),
         pred_en_EJ_flsp = exp(pred_log_en_EJ_flsp)) %>%
  mutate(bias.adder = en_EJ_flsp - pred_en_EJ_flsp) %>%
  select(GCAM_region_ID, fuel, service, bias.adder)

data_tradBio_fin <- data_tradBio %>%
  left_join(data_tradBio_adder, join_by(GCAM_region_ID, fuel, service)) %>%
  mutate(pred_en_EJ_flsp = exp(pred_log_en_EJ_flsp)) %>%
  mutate(pred_en_EJ_flsp_adj = pred_en_EJ_flsp + bias.adder) %>%
  select(GCAM_region_ID, fuel, service, year, en_EJ_flsp, pred_en_EJ_flsp = pred_en_EJ_flsp_adj)

#--------------
# 2- COAL
# Fit function for coal:
# The function should make the demand decrease as income and/or price increase (negative price and income effects)
#formula.coal<- "en_GJ_flsp ~ a * pcgdp_thous^-b * price^-c"
#start.value.coal <- c(a = 1, b = 1, c = 1)

# Use minipack.lm for the estimation (need to check the prediction)
fit_coal_heat <- lm(log_en_EJ_flsp ~ log_pcgdp_thous  + GCAM_region_ID,
               data = coal_heat)

fit_coal_oth <- lm(log_en_EJ_flsp ~ log_pcgdp_thous  + GCAM_region_ID,
                    data = coal_oth)

# Check coefficients
summary(fit_coal_heat)
summary(fit_coal_oth)

# Predict
prelast_coal <- -0.5

data_coal<- serv_coal %>%
  mutate(b1_heat = as.numeric(fit_coal_heat$coefficients[1]),
         b2_heat = as.numeric(fit_coal_heat$coefficients[2]),
         b3_heat = 0, 
         b1_oth = as.numeric(fit_coal_oth$coefficients[1]),
         b2_oth = as.numeric(fit_coal_oth$coefficients[2]),
         b3_oth = 0, 
         prelast = prelast_coal) %>%
  mutate(pred_log_en_EJ_flsp = if_else(grepl("heat", service), 
                                       b1_heat + b2_heat * log_pcgdp_thous + b3_heat * log_sq_pcgdp_thous + prelast_tradBio * log_price,
                                       b1_oth + b2_oth * log_pcgdp_thous + b3_oth * log_sq_pcgdp_thous + prelast_tradBio * log_price))

data_coal_adder <- data_coal %>%
  group_by(GCAM_region_ID) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(en_EJ_flsp = exp(log_en_EJ_flsp),
         pred_en_EJ_flsp = exp(pred_log_en_EJ_flsp)) %>%
  mutate(bias.adder = en_EJ_flsp - pred_en_EJ_flsp) %>%
  select(GCAM_region_ID, fuel, service, bias.adder)

data_coal_fin <- data_coal %>%
  left_join(data_coal_adder, join_by(GCAM_region_ID, fuel, service)) %>%
  mutate(pred_en_EJ_flsp = exp(pred_log_en_EJ_flsp)) %>%
  mutate(pred_en_EJ_flsp_adj = pred_en_EJ_flsp + bias.adder) %>%
  select(GCAM_region_ID, fuel, service, year, en_EJ_flsp, pred_en_EJ_flsp = pred_en_EJ_flsp_adj)






