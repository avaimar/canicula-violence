# 0. Working set up ---------------------------
library(sf)
library(tidyverse)
library(cartography)
library(RColorBrewer)
library(scales)

# 1. Set paths and parameters -----
load("CONFIG.Rspace") # gdrive data paths
processed_data_fpath <- file.path(gdrive_fpath, "Data", "Processed")
figure_fpath <- file.path(gdrive_fpath, "Figures")
figure_fpath <- file.path("results", "figures")
source(file.path("src", "models", "panel_preprocessing_funcs.R"))

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


# 3. Map VHI diff from pre-panel baseline -----

y <- 2015 # choosing 2015 to display

# hacky method for custom div color palette w/ R color Brewer palettes
# custom diverging pal with number in each based on ratio below/above 0
# max(subset(full_sf, Year == y & in_dry_corridor == T)$vhi_diff, na.rm=T)
# min(subset(full_sf, Year == y & in_dry_corridor == T)$vhi_diff, na.rm=T)
divpal <- carto.pal(
  pal1 = "purple.pal", n1 = 13, pal2 = "turquoise.pal", n2 = 3, middle = TRUE,
  transparency = FALSE
)
# function for making palette continuous, so can interpolate over any number of pts
getPalette_Bupu <- colorRampPalette(brewer.pal(9, name = "BuPu"))
#interpolate over 13 pts, and put this in instread of purple
divpal[0:13] <- rev(getPalette_Bupu(13))

# test what color palette looks like
# k <- length(mypal)
# image(1:k, 1, as.matrix(1:k),
#   col = mypal, xlab = paste(k, " classes", sep = ""),
#   ylab = "", xaxt = "n", yaxt = "n", bty = "n"
# )

ggplot() +
  geom_sf(data = country_sf, fill = "gray90", lwd = 0.1, color = "gray20") +
  geom_sf(
    data = subset(full_sf, Year == y & in_dry_corridor == T),
    aes(fill = vhi_diff), lwd = 0.02, color = "gray60"
  ) +
  scale_fill_gradientn(
    colours = divpal,
    na.value = "grey60",
  ) +
  geom_sf(data = country_sf, fill = NA, lwd = 0.1, color = "gray20") +
  guides(fill = guide_colorbar(
    title = "VHI Diff",
    label.theme = element_text(colour = "gray20", angle = 0, size = 8),
    title.theme = element_text(colour = "gray20", angle = 0, size = 10)
  )) +
  theme_void()

ggsave(file.path(figure_fpath, paste0("fig1_vhi_baseline_diff_", y, ".pdf")))

# 4. Map Homicide diff from panel mean -----

y <- 2015 # choosing 2015 to display

#check min/max
# max(subset(full_sf, Year == y & in_dry_corridor == T)$hom_diff, na.rm = T)
# min(subset(full_sf, Year == y & in_dry_corridor == T)$hom_diff, na.rm = T)

min_hom_rate <- -100
max_hom_rate <- 100

ggplot() +
  geom_sf(data = country_sf, fill = "gray90", lwd = 0.1, color = "gray20") +
  geom_sf(
    data = subset(full_sf, Year == y & in_dry_corridor == T),
    aes(fill = pmin(hom_diff, max_hom_rate)), lwd = 0.02, color = "gray60"
  ) +
  geom_sf(data = country_sf, fill = NA, lwd = 0.1, color = "gray20") +
  scale_fill_gradient2(
    high = muted("red"), low = muted("blue"), mid = "white",
    midpoint = 0, na.value = "grey60", limits = c(min_hom_rate, max_hom_rate)
  ) +
  # scale_fill_viridis_c(option = "magma", na.value = "grey40", begin = 1, end = .5) +
  guides(fill = guide_colorbar(
    title = "Homicide Rate (per 100K)",
    label.theme = element_text(colour = "gray20", angle = 0, size = 8),
    title.theme = element_text(colour = "gray20", angle = 0, size = 10)
  )) +
  theme_void()
ggsave(file.path(figure_fpath, paste0("fig1_homrate_baseline_diff", y, ".pdf")))


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