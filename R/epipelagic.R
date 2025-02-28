essai <- station_sp %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "station") %>% 
  inner_join(metadata %>% select(station, depth), by = "station") %>% 
  mutate(
    depth_layer = case_when(
      between(depth, 0, 174) ~ "Epipelagic",
      between(depth, 175, 699) ~ "Upper mesopelagic",
      between(depth, 700, 999) ~ "Lower mesopelagic",
      between(depth, 1000, 2000) ~ "Bathypelagic"
    )
  ) %>% 
  tidyr::pivot_longer(!c(depth_layer, depth, station), names_to = "species",
                      values_to = "biomass") %>% 
  filter(depth_layer == "Epipelagic" & biomass > 0) %>% 
  group_by(station) %>% 
  mutate(n = n_distinct(species)) %>% 
  inner_join(sp_ui, by = "species") 

essai_P1062 <- essai %>%
  filter(station == "P1062") %>%
  select(station, species) %>%
  distinct()

essai_autres_stations <- essai %>%
  filter(station != "P1062") %>%
  select(station, species) %>%
  distinct()

essai_uniques_P1062 <- essai_P1062 %>%
  anti_join(essai_autres_stations, by = "species")

# View the unique species
P1062_ui <- essai_uniques_P1062 %>% 
  inner_join(sp_ui, by = "species") 



ses_epi <- SES_results_station %>% 
  filter(depth_layer=="Epipelagic" & index=="Functional dispersion")
