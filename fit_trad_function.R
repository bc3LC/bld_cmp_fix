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
  mutate(en_GJ = en_EJ * 1E9,
         en_GJ_flsp = en_GJ / flsp) %>%
  filter(en_EJ > 1E-4)


serv_TradBio <- read.csv("input/serv_TradBio.csv") %>%
  filter(year >= 1975) %>%
  as_tibble() %>%
  left_join_error_no_match(flsp, by = join_by(GCAM_region_ID, year)) %>%
  # Transform to GJ
  mutate(en_GJ = en_EJ * 1E9,
         en_GJ_flsp = en_GJ / flsp) %>%
  filter(en_GJ_flsp != 0)
  
#--------------
# 1- TRADITIONAL BIOMASS
# Fit function for traditional biomass:
# Given its carbon neutrality and that the price is exogenous (not linked with the land module, we can drop the price effect out)

formula.tradBio<- "en_GJ_flsp ~ x / (pcgdp_thous)"
start.value.tradBio <- c(x = 1)

# Use minipack.lm for the estimation (need to check the prediction)
fit_tradBio = minpack.lm::nlsLM(formula.tradBio, serv_TradBio, start = start.value.tradBio)

# Check coefficients
summary(fit_tradBio)

# Check R2
fit_tradBio_data <- fit_tradBio$m

Residuals_tradBio <- fit_tradBio_data$resid()
SumResSquared_tradBio <- sum(Residuals_tradBio^2)
TotalSumSquares_tradBio <- sum((serv_TradBio$en_GJ_flsp - mean(serv_TradBio$en_GJ_flsp))^2)
RSquared_tradBio <- 1 - (SumResSquared_tradBio / TotalSumSquares_tradBio)
RSquared_tradBio

# Predicted df
predicted_serv_TradBio <- serv_TradBio %>%
  mutate(x = coef(fit_tradBio),
         en_GJ_flsp_pred = x / pcgdp_thous,
         en_EJ_pred = en_GJ_flsp_pred * flsp / 1E9)

# Plot
ggplot(serv_TradBio, aes(x = pcgdp_thous, y = en_EJ)) + 
  geom_point() +
  theme_bw() +
  labs(x = "Per capita GDP (thous$/pers)", y = ("GJ_flsp")) + 
  geom_point(color='red', data = predicted_serv_TradBio, aes(x = pcgdp_thous, y = en_EJ_pred), pch = 4) + 
  ggtitle("Fitted function for Traditional Biomass demand in the residential sector",
          subtitle = "Service_GJ_flsp ~ x / pcgdp_thous")

ggsave("figures/Fit_TradBio.tiff", last_plot(), "tiff")

#--------------
# 2- COAL
# Fit function for coal:
# The function should make the demand decrease as income and/or price increase (negative price and income effects)

formula.coal<- "en_GJ_flsp  ~ (A / pcgdp_thous) + (k / price)"

# Manual starting values
start.value.coal <- c(A = 0, k = 0)

# Use minipack.lm for the estimation (need to check the prediction)
fit_coal = minpack.lm::nlsLM(formula.coal, serv_coal, start = start.value.coal)

# Check coefficients
summary(fit_coal)

# Check R2
fit_coal_data <- fit_coal$m

Residuals_coal <- fit_coal_data$resid()
SumResSquared_coal <- sum(Residuals_coal^2)
TotalSumSquares_coal <- sum((serv_coal$en_GJ_flsp - mean(serv_coal$en_GJ_flsp))^2)
RSquared_coal <- 1 - (SumResSquared_coal / TotalSumSquares_coal)
RSquared_coal

A_coal<-coef(fit_coal)
#k_coal<-coef(fit_coal)[2]

# Predicted df
predicted_serv_coal <- serv_coal %>%
  mutate(A = coef(fit_coal),
         en_GJ_flsp_pred = A / (pcgdp_thous * price),
         en_EJ_pred = en_GJ_flsp_pred * flsp / 1E9)

# Plot
ggplot(serv_coal, aes(x = pcgdp_thous, y = en_EJ)) + 
  geom_point() +
  theme_bw() +
  labs(x = "Per capita GDP (thous$/pers)", y = ("GJ_flsp")) + 
  geom_point(color='red', data = predicted_serv_coal, aes(x = pcgdp_thous, y = en_EJ_pred)) + 
  ggtitle("Fitted function for Coal demand in the residential sector",
          subtitle = "Service_GJ_flsp ~ x / pcgdp_thous")

ggsave("Fit_Coal.tiff", last_plot(), "tiff")

