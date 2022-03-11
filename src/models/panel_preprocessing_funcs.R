add_vhi_panel_vars <- function(vhi_df) {
  # convert canicula index to ordered factor
  # make unique identifier for each spatial unit as Municipality_Department
  # make vhi on scale from 1-100 to make coefs easier to interpret
  canicula_labels <- c("Extreme", "Severe", "Moderate", "Mild", "None")
  vhi_df %>%
    mutate(
      Canicula_Label = factor(Canicula_Label, labels = canicula_labels),
      munic_dep = paste(Municipio, Departamento, sep = "_"),
      mean_vhi = mean_vhi * 100
    )
}

pre_panel_vhi <- function(vhi_df, panel_start_year) {
  # for each spatial unit, add a pre_panel_vhi column of mean of all years vhi
  # before panel start year, and subset output df to panel period
  pre_panel_vhi <- vhi_df %>%
    filter(Year < panel_start_year) %>%
    group_by(shapeID) %>%
    summarise(pre_panel_vhi = mean(mean_vhi))
  vhi_df %>%
    filter(Year >= panel_start_year) %>%
    left_join(pre_panel_vhi) %>%
    mutate(vhi_diff = mean_vhi - pre_panel_vhi) # diff from pre-panel baseline
}


join_panel <- function(admin_bndry_sf, vhi_df, hom_df) {
  # Join Canicula Data, Homicide Data, and Shapefiles
  canicula_sf <- left_join(
    admin_bndry_sf,
    vhi_df,
    by = c("shapeID", "Departamento", "Municipio", "shapeGroup")
  )

  full_sf <- left_join(
    canicula_sf,
    hom_df,
    by = c("Departamento", "Municipio", "Year")
  )
}

remove_dup_spatial_units <- function(full_sf) {
  # get the 3 munic-departments that are non-unique due to
  # Null Municipality or weird multiple shapeID issue
  # TODO:for now we will drop these, but need to investigate these later

  # get non-spatial DF so that we can determine duplicates
  # without considering geometry
  full_df <- st_drop_geometry(full_sf)
  nonunique_munic_dept <- full_df[duplicated(full_df[, c("munic_dep", "Year")]), ] %>%
    ungroup() %>%
    dplyr::select(munic_dep) %>%
    unique() %>%
    unlist()
  print("Dropping dup spatial units...")
  print(nonunique_munic_dept)
  full_sf %>% filter(!(munic_dep %in% nonunique_munic_dept))
}

add_mean_panel_homicide <- function(full_sf) {
  # get difference in homicides from panel mean for each spatial unit
  homicide_avgs <- full_sf %>%
    st_drop_geometry() %>%
    group_by(munic_dep) %>%
    summarize(hom_panel_mean = mean(hom_rate_100k))

  full_sf %>%
    left_join(homicide_avgs) %>%
    mutate(hom_diff = hom_rate_100k - hom_panel_mean)
}

build_clean_panel_sf <- function(
  admin_bndry_sf, vhi_df, hom_df, panel_start_year = 2013) {
  # add/transform needed panel vars
  vhi_df <- add_vhi_panel_vars(vhi_df)
  vhi_df <- pre_panel_vhi(vhi_df, panel_start_year = panel_start_year)

  full_sf <- join_panel(admin_bndry_sf, vhi_df, hom_df)
  full_sf <- remove_dup_spatial_units(full_sf)
  add_mean_panel_homicide(full_sf)
}