# 0. Working set up ---------------------------
library(ggplot2)
library(sf)
library(tidyverse)
library(fixest)
library(dplyr)

gdrive_fpath <- "/Volumes/GoogleDrive/My Drive/Stanford/ESS 268/Project"
model_data_fpath <- file.path(gdrive_fpath, "Models")
figure_fpath <- file.path("results", "figures" )

load(file.path(model_data_fpath, "modelmatrix.RData"))

# 1. Plot -------------------------------------

# Get total annual homicides 
full_df <- mutate(full_df, homicides=hom_rate_100k* Population / 100000)
annual_hom <- full_df %>% 
  group_by(Year) %>% 
  dplyr::summarize(homicides=sum(homicides, na.rm=T))
annual_hom <- annual_hom %>% mutate(Scenario='Actual')

ggplot() +
  geom_line(data=annual_hom, aes(x=Year, y=homicides)) +
  theme_classic()

# Predict annual counterfactual for each municipality
CF_YEAR <- 2017
cf_df <- full_df
cf_df <- cf_df %>% select(munic_dep, Year, Population, mean_vhi, hom_rate_100k)

base_df <- cf_df %>% filter(Year == CF_YEAR) %>%
  mutate(base_vhi=mean_vhi) %>%
  select(-c(mean_vhi, Population, hom_rate_100k, Year))

cf_df <- left_join(cf_df, base_df, by=c('munic_dep'))

# colSums(is.na(cf_df))
predict_homicides <- function(munic_dep, Year, vhi) {
  predict(m1.vhi, data.frame(munic_dep=munic_dep, Year=Year, mean_vhi=vhi))
}

cf_df <- cf_df %>% rowwise() %>%
  mutate(predicted_hom = predict_homicides(munic_dep, Year, base_vhi))

# Remove negative rates
cf_df <- mutate(cf_df, predicted_hom = ifelse(predicted_hom < 0, 0, predicted_hom))

# Set chosen CF year to actual values
cf_df <- cf_df %>%
  mutate(
    predicted_hom = case_when(
    Year == CF_YEAR ~ hom_rate_100k,
    TRUE ~ predicted_hom)
  )

# Compute annual counterfactual
cf_df <- mutate(cf_df, homicides_cf=predicted_hom* Population / 100000)
annual_hom_cf <- cf_df %>% 
  group_by(Year) %>% 
  dplyr::summarize(homicides_cf=sum(homicides_cf, na.rm=T))

annual_hom_cf <- annual_hom_cf %>% 
  mutate(Scenario='Counterfactual') %>%
  mutate(homicides=homicides_cf) %>%
  select(Year, homicides, Scenario)

# Bind data for plotting
final <- rbind(annual_hom, annual_hom_cf)

vals <- c("Actual" = "grey47", "Counterfactual" = "lightcoral")
ggplot(data=final, aes(x=Year, y=homicides, group=Scenario, colour=Scenario)) +
  geom_line() +
  theme_classic() + 
  theme(legend.position='bottom') + 
  scale_color_manual(values=vals)

ggplot(data=cf_df, aes(x=mean_vhi, group=Year))+ 
  geom_histogram(aes(group=Year)) + 
  facet_grid(Year~.)
