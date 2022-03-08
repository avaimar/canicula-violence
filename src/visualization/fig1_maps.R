# 0. Working set up ---------------------------
library(sf)
library(tidyverse)
library(cartography)
library(RColorBrewer)
library(scales)

#1. Set paths and parameters -----
load("CONFIG.Rspace") # gdrive data paths
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
#figure_fpath <- file.path(gdrive_fpath, "Figures")
figure_fpath <- file.path("results", "figures" )
source(file.path('src', 'models', 'panel_preprocessing_funcs.R'))

# 1. Read in Data -----
country_sf <- st_read(
  file.path(processed_data_fpath, "country_outlines.geojson")
)
admin_bndry_sf <- st_read(
  file.path(processed_data_fpath, "cleaned_admin_boundaries.geojson")
)
vhi_df <- read.csv(
  file.path(processed_data_fpath, "Canicula_index_1982_2020.csv")
)
hom_df <- read.csv(file.path(processed_data_fpath, "homicide_rates.csv"))

# 2. Build Clean Panel Data -----
full_sf <- build_clean_panel_sf(admin_bndry_sf, vhi_df, hom_df, panel_start_year = 2013)

# 3. Build Fig 1 Maps -----
full_sf <- full_sf %>% 
  mutate(vhi_diff = mean_vhi - pre_panel_vhi) #get diff from pre-panel baseline

y <- 2015 # choosing 2015 to display

getPalette_Bu <- colorRampPalette(brewer.pal(9, name = "BuPu"))
purp_20 <- getPalette_Bu(20)
purp5 <- c(purp_20[1], purp_20[7], purp_20[11], purp_20[16], purp_20[20])

pubugn_pal <- c(rev(brewer.pal(5, name = "BuPu")), brewer.pal(5, name = "Greens"))

#Mean VHI (over 9 week period in June/July) Map
y <- 2015
ggplot() +
  geom_sf(data = country_sf, fill = "gray90", lwd = 0.1, color = "gray20") +
  geom_sf(
    data = subset(full_sf, Year == y & in_dry_corridor == T),
    aes(fill =  vhi_diff), lwd = 0.02, color = "gray60"
  ) +
  scale_fill_gradientn(
    colours = pubugn_pal,
    limits = c(-50, 50),
    na.value = "grey60"
  ) +
  # scale_fill_gradient2(
  #   low = brewer.pal(8, name = "BuPu")[8], mid = 'white', high = brewer.pal(8, name = "Greens")[8],
  #   limits = c(-50, 50),
  #   na.value = "grey60"
  # ) +
  geom_sf(data = country_sf, fill = NA, lwd = 0.1, color = "gray20") +
  # scale_fill_viridis_c(option = "magma", na.value = "grey40", begin = 1, end = .5) +
  guides(fill = guide_colorbar(
    title = "VHI Diff",
    label.theme = element_text(colour = "gray20", angle = 0, size = 8),
    title.theme = element_text(colour = "gray20", angle = 0, size = 10)
  )) +
  theme_void()
ggsave(file.path(figure_fpath, paste0("VHI_Baseline_Diff_", y, ".pdf")))


##TODO: change this to be from homicide baseline
homicide_avgs <- full_sf %>% 
  group_by(Departamento, Municipio) %>% 
  summarize(hom_avg = mean(hom_rate_100k)) %>% 
  as.data.frame() %>%
  select(-geometry)

full_sf <- left_join(full_sf, homicide_avgs, by = c('Departamento', 'Municipio'))
full_sf <- full_sf %>% 
  mutate(hom_diff = hom_rate_100k - hom_avg)

# Homicide rate map
min_hom_rate <- -100
max_hom_rate <- 100
hom_rate_ceil <- 100

ggplot() +
  geom_sf(data = country_sf, fill = "gray90", lwd = 0.1, color = "gray20") +
  geom_sf(
    data = subset(full_sf, Year == y & in_dry_corridor == T),
    aes(fill = pmin(hom_diff, hom_rate_ceil)), lwd = 0.02, color = "gray60"
  ) +
  geom_sf(data = country_sf, fill = NA, lwd = 0.1, color = "gray20") +
  scale_fill_gradient2(
    high = muted("red"), low = muted('blue'), mid = "white",
    midpoint = 0, na.value = "grey60", limits = c(min_hom_rate,max_hom_rate)) +
  # scale_fill_viridis_c(option = "magma", na.value = "grey40", begin = 1, end = .5) +
  guides(fill = guide_colorbar(
    title = "Homicide Rate (per 100,000)",
    label.theme = element_text(colour = "gray20", angle = 0, size = 8),
    title.theme = element_text(colour = "gray20", angle = 0, size = 10)
  )) +
  theme_void()
ggsave(file.path(figure_fpath, paste("Homicide_Rate_Ceil_Dry_", y, ".pdf", sep = "")))



# # Canicula Index Map with Labels
# ggplot() +
#   geom_sf(data = country_sf, fill = "gray90", lwd = 0.1, color = "gray20") +
#   geom_sf(
#     data = subset(full_sf, Year == y & in_dry_corridor == T),
#     aes(fill = Canicula_Label), lwd = 0.02, color = "gray60"
#   ) +
#   scale_fill_manual(values = rev(purp5), na.value = "grey80") +
#   geom_sf(data = country_sf, fill = NA, lwd = 0.12, color = "gray20") +
#   guides(fill = guide_legend(
#     title = "Canicula Intensity",
#     label.theme = element_text(colour = "gray20", angle = 0, size = 8),
#     title.theme = element_text(colour = "gray20", angle = 0, size = 10)
#   )) +
#   theme_void()
# ggsave(file.path(figure_fpath, paste("Canicula_Label_Dry_", y, ".pdf", sep = "")))
# 

       