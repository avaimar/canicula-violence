# 0. Working set up ---------------------------
require(ggplot2)
require(sf)
require(tidyverse)
require(fixest)
require(dplyr)
require(RColorBrewer)
require(scales)
require(reshape2)
require(svglite)

# gdrive_fpath <- "/Volumes/GoogleDrive/My Drive/Stanford/ESS 268/Project"
gdrive_fpath <- file.path( ##change this to point at google drive path
  "/Volumes/GoogleDrive/My Drive/Project")
model_data_fpath <- file.path(gdrive_fpath, "Models")
raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
figure_fpath_local <- file.path("results", "figures")
#figure_fpath <- file.path(gdrive_fpath, "figures" )

load(file.path(model_data_fpath, "modelmatrix_dry.RData"))

admin_bndry_sf <- st_read(
  file.path(processed_data_fpath, "cleaned_admin_boundaries.geojson"))

country_sf <- st_read(
  file.path(processed_data_fpath, "country_outlines.geojson")
)

# 1. Plot -------------------------------------

# Predict annual counterfactual for each municipality
CF_YEAR <- 2017
cf_df <- full_df
cf_df <- cf_df %>% dplyr::select(Country, in_dry_corridor, Departamento, munic_dep, Year, Population, mean_vhi, hom_rate_100k)

base_df <- cf_df %>% filter(Year == CF_YEAR) %>%
  mutate(base_vhi=mean_vhi) %>%
  dplyr::select(-c(mean_vhi, Population, hom_rate_100k, Year, Country, in_dry_corridor, Departamento))

cf_df <- left_join(cf_df, base_df, by = c("munic_dep"))

# colSums(is.na(cf_df))
predict_homicides <- function(munic_dep, Year, vhi) {
  predict(m1.vhi, data.frame(munic_dep = munic_dep, Year = Year, mean_vhi = vhi))
}

cf_df <- cf_df %>% rowwise() %>%
  mutate(predicted_hom = predict_homicides(munic_dep, Year, base_vhi))

# Remove negative rates
cf_df <- mutate(cf_df,
    predicted_hom = ifelse(predicted_hom < 0, 0, predicted_hom),
    hom_rate_change = ifelse(is.na(hom_rate_100k), NA, hom_rate_100k - predicted_hom))

admin_bndry_sf$munic_dep <- paste(
    admin_bndry_sf$Municipio, admin_bndry_sf$Departamento, sep = "_")

cf_sf <- left_join(admin_bndry_sf, cf_df, by=c('munic_dep'))



min_hom_rate <- -100
max_hom_rate <- 100

y <- 2015
cf_sf %>% filter(Year == y, in_dry_corridor.x == T) %>%
    ggplot() +
  geom_sf(data = country_sf, fill = "white", lwd = 0.12, color = "gray20") +
    geom_sf(aes(fill = pmin(hom_rate_change, max_hom_rate)), lwd = .04, color = "gray75") +
    scale_fill_gradient2(
        high = muted("red"), low = muted('blue'), mid = "white",
        midpoint = 0, na.value = "grey75", limits = c(min_hom_rate,max_hom_rate)) +
    geom_sf(data = country_sf, fill = NA, lwd = 0.12, color = "gray20") +
    guides(fill = guide_colorbar(
      title = 'Predicted Homicide Rate Change (per 100,000)',
      label.theme = element_text(colour = "gray10", angle = 0, size = 8), 
      title.theme = element_text(colour = "gray10", angle = 0, size = 10))) + 
  theme_void()
ggsave(file.path(figure_fpath, paste("fig2_panB_homrate_change_dry_", y, ".pdf", sep = "")))


cf_df_department <- cf_df %>% 
  filter(!is.na(predicted_hom), !is.na(hom_rate_100k), predicted_hom != 0, hom_rate_100k != 0) %>%
  mutate(Population = ifelse(Country == 'Nicaragua', 1, Population)) %>%
  group_by(Departamento, Country, Year) %>%
  summarise(hom_rate_100k =  weighted.mean(hom_rate_100k, Population),
          predicted_hom =weighted.mean(predicted_hom, Population), 
          Population = sum(Population))
  # filter(Year == 2014) %>%
    # mutate(hom_rate_100k = pmin(300, hom_rate_100k)) %>%
  # filter(Country != 'Nicaragua') %>%
cf_df_department %>% filter(Country != 'Nicaragua') %>%
  ggplot() +
    geom_point(
      aes(y = hom_rate_100k, x = predicted_hom, color = Country, size = Population), shape = 21) + 
    # scale_x_continuous(limits = c(0, 400)) +
    # scale_y_continuous(limits = c(0, 200)) +
    geom_abline(slope = 1) +
    facet_wrap(~Year, ncol = 2, scales = "free") + 
    theme_bw() + 
    theme(panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    axis.line = element_line(colour = "black"))
  ggsave(file.path(figure_fpath, 'pred_vs_actl_department.pdf'))

cols = c('#fb49b0', '#537eff', '#00b25d')

  # filter(Country != 'Nicaragua') %>%
cf_df_department %>% filter( Country != 'Nicaragua', Year %in% c(2013, 2014, 2015)) %>%
  ggplot() +
    geom_point(
      aes(y = hom_rate_100k, x = predicted_hom, color = Country, size = Population), shape = 21) + 
    scale_color_manual(values = cols)+
    # scale_x_continuous(limits = c(0, 400)) +
    # scale_y_continuous(limits = c(0, 200)) +
    geom_abline(slope = 1) +
    labs(x = "Predicted Homicide Rate (2017 VHI)", y = "Actual Homicide Rate")+
        facet_wrap(~Year, nrow = 1, scales = 'free')+
    theme_classic() + 
    theme(panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    legend.position="bottom")
  ggsave(file.path(figure_fpath_local, 'pred_vs_actl_department_2015-2017.pdf'), height = 3.5, width = 6)