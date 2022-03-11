# 0. Working set up ---------------------------
library(ggplot2)
library(sf)
library(tidyverse)
library(fixest)
library(dplyr)
library(reshape2)

#1. Set paths and parameters -----
load('CONFIG.Rspace')
model_data_fpath <- file.path(gdrive_fpath, "Models")
result_tables_fpath <- file.path("results", "tables")
figure_fpath <- file.path(gdrive_fpath,  "Figures")
figure_fpath <- file.path("results", "figures")

load(file.path(model_data_fpath, "modelmatrix.RData"))


# 1. Predict Counterfactuals ------------
full_df <- st_drop_geometry(full_sf)

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
# colSums(is.na(cf_df))
predict_homicides <- function(model, munic_dep, Year, vhi) {
  predict(model, data.frame(munic_dep=munic_dep, Year=Year, mean_vhi=vhi))
}

cf_df <- full_df %>%
  dplyr::select(munic_dep, Year, Population, mean_vhi, hom_rate_100k, pre_panel_vhi) %>% 
  rowwise() %>%
  mutate(predicted_hom = predict_homicides(m1.vhi, munic_dep, Year, pre_panel_vhi))

# Clip negative rates
cf_df <- mutate(cf_df, predicted_hom = ifelse(predicted_hom < 0, 0, predicted_hom))

# Compute annual counterfactual (aggregate)
cf_df <- mutate(cf_df, homicides_cf=predicted_hom* Population / 100000)
annual_hom_cf <- cf_df %>% 
  group_by(Year) %>% 
  dplyr::summarize(homicides_cf=sum(homicides_cf, na.rm=T))

annual_hom_cf <- annual_hom_cf %>% 
  mutate(Scenario='Counterfactual') %>%
  mutate(
    homicides=homicides_cf) %>%
  dplyr::select(Year, homicides, Scenario)
  
# 2. Compute Counterfactuals CIs and Rate Densities ------------
# this takes a while.... can def be made more computationally efficient if needed
set.seed(42)
cf_hom <- matrix(nrow=100, ncol=8) # Save each year
cf_rate_densities <- cf_df %>% dplyr::select(munic_dep, Year, hom_rate_100k, pre_panel_vhi)
ll = dim(full_df)[1]

for (i in 1:100)  {
  if (i %% 10 == 0) print(i)
  # Fit model on bootstrapped dataset
  samp <- sample(1:ll,size=ll,replace=T)
  newdata = full_df[samp,]
  mod <- feols(hom_rate_100k ~ mean_vhi |  munic_dep + Year, data = newdata)
  
  # Get predictions 
  bs_df <- cf_df %>% rowwise() %>%
    mutate(predicted_hom = predict_homicides(mod, munic_dep, Year, pre_panel_vhi))
  
  # Remove negative rates
  bs_df <- mutate(bs_df, predicted_hom = ifelse(predicted_hom < 0, 0, predicted_hom))

  # Compute annual counterfactual
  bs_df <- mutate(bs_df, homicides_cf=predicted_hom* Population / 100000)
  annual_hom_bs <- bs_df %>% 
    group_by(Year) %>% 
    dplyr::summarize(homicides_cf=sum(homicides_cf, na.rm=T))
  
  # Append predicted rate change to densities data.frame
  bs_df <- bs_df %>% mutate(predicted_rate_change = predicted_hom - hom_rate_100k)
  cf_rate_densities <- cbind(
    cf_rate_densities,
    dplyr::select(bs_df, predicted_rate_change))
  
  # Save 
  cf_hom[i,] <- pull(annual_hom_bs,  homicides_cf)
}
#save bootstrapped samples
colnames(cf_hom) <- 2013:2020
write.csv(cf_hom, file.path(result_tables_fpath, 'baseline_counterfactual_boots.csv' ))

# 3. Plot Timeseries ------------
# Extract 95% CI
confint <- apply(cf_hom, 2,function(x) quantile(x,probs=c(0.05, 0.95))) 
confint <- t(confint)
confint <- as.data.frame(confint)
confint$Year <- 2013:2020

confint <- reshape2::melt(confint, id.vars='Year', variable.name='Scenario', value.name='homicides')
confint <- confint %>%  dplyr::select(Year, homicides, Scenario)

# Bind data for plotting
final <- rbind(annual_hom, annual_hom_cf, confint)
finaldc <- reshape2::dcast(final, formula=Year ~ Scenario, value.var='homicides')

cfac_color <- blue_pal[6]

g1 <- ggplot() +
  geom_line(data=finaldc,
            aes(x=Year, y=Actual/1000), colour='black', lwd = .6) +
  geom_line(data=finaldc,
            aes(x=Year, y=Counterfactual/1000), colour=cfac_color, lwd = .6) +
  theme_classic() + 
  labs(y='Total Homicides (1K)') + 
  theme_classic() + 
  geom_ribbon(data=finaldc,
              aes(x=Year, ymin = `5%`/1000, ymax = `95%`/1000), alpha = 0.4, fill = cfac_color)

ggsave(file.path(figure_fpath, 'fig3_timeseries.pdf'), g1, scale=0.8, width = 4, height = 3.5)



# 4. Plot pdfs ------------
colnames(cf_rate_densities) <- c(
  'munic_dep', 'Year', 'hom_rate_100k', 'pre_panel_vhi', 
  1:(dim(cf_rate_densities)[2] -4))

cf_rate_densities <- cf_rate_densities %>% 
  dplyr::select(-c(munic_dep, Year, hom_rate_100k, pre_panel_vhi))

cf_rate_densities <- as.matrix(cf_rate_densities)
cf_rate_densities <- apply(cf_rate_densities, 1,function(x) quantile(x,probs=c(0.05, 0.95), na.rm=T)) 

cf_rate_densities <- cbind(
  dplyr::select(cf_df, munic_dep, Year, Population, mean_vhi, hom_rate_100k, predicted_hom), 
  t(cf_rate_densities)
)

cf_rate_densities <- cf_rate_densities %>% 
  mutate(rate_chg_5_point = predicted_hom - hom_rate_100k)

# Plot
#Note: we remove extreme outliers
g2 <- ggplot(data=filter(cf_rate_densities, `5%` > -75 & `95%` < 70), aes(group=Year))+ 
  geom_density(aes(x=rate_chg_5_point, group=Year), color = 'black') + 
  geom_density(aes(x=`5%`, group=Year), fill=cfac_color, alpha=.4, color = NA) + 
  geom_density(aes(x=`95%`, group=Year), fill=cfac_color, alpha=.4, color = NA) + 
  facet_grid(Year~.) + 
  theme_classic() +
  labs(y='', x='Predicted Homicide Rate Change') + 
  theme(axis.text.y = element_blank(), axis.ticks.y=element_blank(),
        axis.line.y = element_blank()) +
  geom_vline(xintercept=0, color = 'white')
ggsave(file.path(figure_fpath, 'fig3_densities.pdf'), g2, scale=0.8, width = 4, height = 10)
