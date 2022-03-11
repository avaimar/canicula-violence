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

# 1. Set paths and parameters -----
load("CONFIG.Rspace")
model_data_fpath <- file.path(gdrive_fpath, "Models")
raw_data_fpath <- file.path(gdrive_fpath, "Data", "Raw")
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
figure_fpath_local <- file.path("results", "figures")
# figure_fpath <- file.path(gdrive_fpath, "figures" )

# load panel sf and fixed effect model
load(file.path(model_data_fpath, "modelmatrix.RData"))

admin_bndry_sf <- st_read(
  file.path(processed_data_fpath, "cleaned_admin_boundaries.geojson")
)

country_sf <- st_read(
  file.path(processed_data_fpath, "country_outlines.geojson")
)

# 1. Predict Counterfactuals  ----------------

# Predict annual counterfactual for each municipality
predict_homicides <- function(munic_dep, Year, vhi) {
  predict(
    m1.vhi,
    data.frame(munic_dep = munic_dep, Year = Year, mean_vhi = vhi)
  )
}

cf_sf <- full_sf %>%
  rowwise() %>%
  mutate(predicted_hom = predict_homicides(munic_dep, Year, pre_panel_vhi))

# Remove negative rates
cf_sf <- mutate(cf_sf,
  predicted_hom = ifelse(predicted_hom < 0, 0, predicted_hom),
  hom_rate_change = hom_rate_100k - predicted_hom
)


# 2. Plot Counterfactual hom diff map  ---------------
min_hom_rate <- -100
max_hom_rate <- 100

y <- 2015
cf_sf %>%
  filter(Year == y, in_dry_corridor == T) %>%
  ggplot() +
  geom_sf(data = country_sf, fill = "white", lwd = 0.12, color = "gray20") +
  geom_sf(
    aes(fill = pmin(hom_rate_change, max_hom_rate)),
    lwd = .04,
    color = "gray75"
  ) +
  scale_fill_gradient2(
    high = muted("red"), low = muted("blue"), mid = "white",
    midpoint = 0, na.value = "grey75",
    limits = c(min_hom_rate, max_hom_rate)
  ) +
  geom_sf(data = country_sf, fill = NA, lwd = 0.12, color = "gray20") +
  guides(fill = guide_colorbar(
    title = "Pred",
    label.theme = element_text(colour = "gray10", angle = 0, size = 8),
    title.theme = element_text(colour = "gray10", angle = 0, size = 10)
  )) +
  theme_void()
ggsave(
  file.path(figure_fpath_local, paste0("fig3_homrate_diff_map_", y, ".pdf")),
  height = 9, width = 12
)


# 2. Plot Department level Pred vs. Actl Scatterplot  ---------------

# get counterfactual/actual homicide rate and populaton for each dept
cf_sf_department <- cf_sf %>%
  filter(
    !is.na(predicted_hom),
    !is.na(hom_rate_100k),
    predicted_hom != 0,
    hom_rate_100k != 0
  ) %>%
  mutate(Population = ifelse(Country == "Nicaragua", 1, Population)) %>%
  group_by(Departamento, Country, Year) %>%
  summarise(
    hom_rate_100k = weighted.mean(hom_rate_100k, Population),
    predicted_hom = weighted.mean(predicted_hom, Population),
    Population = sum(Population)
  )

# plot all years
cf_sf_department %>%
  filter(Country != "Nicaragua") %>%
  ggplot() +
  geom_point(
    aes(y = hom_rate_100k, x = predicted_hom, color = Country, size = Population),
    shape = 21
  ) +
  geom_abline(slope = 1) +
  facet_wrap(~Year, ncol = 2, scales = "free") +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  )

ggsave(
  file.path(figure_fpath, "fig3_pred_vs_actl_department.pdf"),
  height = 8, width = 6
)

# plot 2013-2015 with consistent colormap
red <- "#C42424" # lightened version of muted("red")
purp <- brewer.pal(9, name = "BuPu")[8]
blue <- brewer.pal(9, name = "BuPu")[4]
pal <- c(red, purp, blue)

# filter(Country != "Nicaragua") %>%
cf_sf_department %>%
  filter(
    Country != "Nicaragua", Year %in% c(2013, 2014, 2015)
  ) %>%
  ggplot() +
  geom_point(
    aes(y = hom_rate_100k, x = predicted_hom, color = Country, size = Population),
    shape = 21
  ) +
  scale_color_manual(values = pal[1:3]) +
  # scale_y_continuous(limits = c(0, 200)) +
  geom_abline(slope = 1) +
  labs(x = "Predicted Homicide Rate (2017 VHI)", y = "Actual Homicide Rate") +
  facet_wrap(~Year, nrow = 1, scales = "fixed") +
  theme_classic() +
  theme(
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
ggsave(
  file.path(figure_fpath_local, "fig3_pred_vs_actl_department_2015_2017_shared.pdf"),
  height = 3.5, width = 5.5
)