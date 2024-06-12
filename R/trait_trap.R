library(traitstrap)

# Trait 
trait_boot <- morpho_data[,1:23] %>% 
  inner_join(metadata) %>% 
  select(-c(individual_code, years, longitude_start,
            latitude_start, longitude_end, longitude_end,
            volume_filtered, distance)) %>% 
  mutate(
    eye_size = eye_diameter / head_depth,
    orbital_length = eye_diameter / standard_length,
    oral_gap_surface = mouth_width * mouth_depth / body_width * body_depth,
    oral_gape_shape = mouth_depth / mouth_width,
    oral_gape_position = distance_upper_jaws_bottom_head / head_depth,
    lower_jaw_length = lower_jaw_length / standard_length,
    head_length = head_length / standard_length,
    body_depth = body_depth / standard_length,
    pectoral_fin_position = distance_pectoral_bottom_body / body_depth_pectoral_insertion,
    pectoral_fin_insertion = prepectoral_length / standard_length,
    transversal_shape = body_depth / body_width,
    dorsal_fin_insertion = predorsal_length / standard_length,
    eye_position = eye_height / head_depth,
    operculum_volume = operculum_depth / operculum_width,
    gill_outflow = operculum_width,
    caudal_throttle_width = caudal_peduncle_min_depth
  ) %>%
  select(
    depth,
    species,
    eye_size,
    orbital_length,
    gill_outflow,
    oral_gap_surface,
    oral_gape_shape,
    oral_gape_position,
    lower_jaw_length,
    head_length,
    body_depth,
    pectoral_fin_position,
    pectoral_fin_insertion,
    transversal_shape,
    caudal_throttle_width,
    dorsal_fin_insertion,
    eye_position,
    operculum_volume
  ) %>% 
  tidyr::pivot_longer(!c(species,depth), names_to = "trait", values_to = "values")%>%
  # add column with depth layer
  mutate(
    depth_layer = case_when(
      between(depth, 0, 174) ~ "Epipelagic",
      between(depth, 175, 699) ~ "Upper mesopelagic",
      between(depth, 700, 999) ~ "Lower mesopelagic",
      between(depth, 1000, 2000) ~ "Bathypelagic"))

# Community 
community <-  rbind(data_biomass_2002_2019, data_biomass_2021, data_biomass_2022)%>%
  as.data.frame()%>%
  rename("species"="Nom_Scientifique",
         "station"="Code_Station") %>%
  left_join(metadata) %>%
  select(species, Tot_V_HV, depth, volume_filtered)%>%
  # add column with depth layer
  mutate(
    depth_layer = case_when(
      between(depth, 0, 174) ~ "Epipelagic",
      between(depth, 175, 699) ~ "Upper mesopelagic",
      between(depth, 700, 999) ~ "Lower mesopelagic",
      between(depth, 1000, 2000) ~ "Bathypelagic"))%>%
  replace(is.na(.), 0)%>%
  group_by(species, depth)%>%
  mutate(biomass=sum(Tot_V_HV))%>%
  select(-c(Tot_V_HV))%>%
  distinct()%>%
  select(-c(volume_filtered)) %>% 
  filter(biomass>0) %>% 
  mutate(species = case_when(
    species == "Nannobrachium_atrum"~"Lampanyctus_ater",
    species == "Cyclothone"~"Cyclothone_sp",
    species == "Stomias_boa_boa"~"Stomias_boa",
    TRUE ~ species
  )) 

trait_filling <- trait_fill(
  # input data (mandatory)
  comm = community,
  traits = trait_boot,
  
  # specifies columns in your data (mandatory)
  abundance_col = "biomass",
  taxon_col = "species",
  trait_col = "trait",
  value_col = "values",
  
  # specifies sampling hierarchy
  scale_hierarchy = c("depth_layer", "depth"),
  
  # min number of samples
  min_n_in_sample = 9
)

trait_filling

# run nonparametric bootstrapping
np_bootstrapped_moments <- trait_np_bootstrap(
  trait_filling, 
  nrep = 100
)

np_bootstrapped_moments


np_bootstrapped_moments$depth_layer <- factor(np_bootstrapped_moments$depth_layer, 
                                              levels = c("Epipelagic", "Upper mesopelagic", "Lower mesopelagic", "Bathypelagic"))

# Mean
ggplot(np_bootstrapped_moments, aes(x=depth_layer, y=mean)) +
  geom_boxplot(aes(col=depth_layer, fill=depth_layer), alpha=0.2) +
  scale_fill_manual(values = c("#FEA520","#D62246","#6255B4","#3C685A"))+
  scale_color_manual(values = c("#FEA520","#D62246","#6255B4","#3C685A"))+
  theme_light() +
  facet_wrap(~trait, scales = "free")


# summarizes bootstrapping output
sum_boot_moment <- trait_summarise_boot_moments(
  np_bootstrapped_moments
)

sum_boot_moment

# run nonparametric bootstrapping
raw_dist_np <- trait_np_bootstrap(
  filled_traits = trait_filling,
  raw = TRUE
)

raw_dist_np
