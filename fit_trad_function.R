library(dplyr)
library(gcamdata)
library(tidyr)
library(ggplot2)

# Load data
serv_coal <- read.csv("input/serv_coal.csv")
serv_TradBio <- read.csv("input/serv_TradBio.csv")
  
#--------------
# 1- TRADITIONAL BIOMASS
# Fit function for traditional biomass:
# Given its carbon neutrality and that the price is exogenous (not linked with the land module, we can drop the price effect out)

formula.tradBio<- "en_EJ ~ x / (pcgdp_thous + y)"
start.value.tradBio<-c(x = 10, y = 10)

fit_tradBio<-nls(formula.tradBio, serv_TradBio, start.value.tradBio) # Correct singular gradient

x_TradBio<-coef(fit_tradBio)[1]
y_TradBio<-coef(fit_tradBio)[2]

#--------------
# 2- COAL
# Fit function for coal:
# The function should make the demand decrease as income and/or price increase (negative price and income effects)

formula.coal<- "en_EJ  ~ (A / pcgdp_thous) + (k / price)"

# Get approximate starting values
#fm0 <- nls(log(en_EJ) ~ log(A/(pcgdp_thous * price + k)), serv_coal, start = c(A = 1, k = 0.1))

# Manual starting values
start.value.coal <- c(A = 1, k = 1)

fit_coal<-nls(formula.coal, serv_coal, start.value.coal) # Correct infinity

A_coal<-coef(fit_coal)[1]
k_coal<-coef(fit_coal)[2]



