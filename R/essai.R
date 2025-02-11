# by depth ----

station_sp <-
  rbind(data_biomass_2002_2019, data_biomass_2021_2022) %>%
  as.data.frame() %>%
  left_join(metadata) %>%
  select(species, biomass_sp, volume_filtered, station) %>%
  # Divide biomass by the volume filtered at each trawl (g.m3)
  mutate(biomass_cpu = (biomass_sp / volume_filtered) * 1000) %>%
  select(species, biomass_cpu, station) %>%
  replace(is.na(.), 0) %>%
  group_by(species, station) %>%
  mutate(biomass = sum(biomass_cpu)) %>%
  select(-biomass_cpu) %>%
  distinct() %>%
  tidyr::pivot_wider(names_from = species, values_from = biomass) %>%
  replace(is.na(.), 0) %>%
  arrange(station) %>%
  filter(!station %in% c("H0411", "L0731", "L0736")) %>%
  tibble::column_to_rownames(var = "station") %>%
  select(order(colnames(.))) %>%
  as.matrix()

# # Calculate functional diversity for the observed data ----
obsFD <- mFD::alpha.fd.multidim(
  sp_faxes_coord = sp_faxes_coord_fish[, c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w = station_sp,
  scaling = TRUE,
  check_input = TRUE,
  details_returned = F
)

results_index <- obsFD$functional_diversity_indices %>% 
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
  select(-station) %>% 
  tidyr::pivot_longer(!c(depth, depth_layer),
                      values_to = "values",
                      names_to = "indice") %>% 
  filter(indice!="sp_richn")

results_index$depth_layer <- factor(
  results_index$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)

ggplot(results_index ,
       aes(x = depth_layer, y = values, fill = depth_layer)) +
  facet_wrap( ~ indice, scales = "free") +
  geom_point(
    alpha = 0.5,
    size = 1,
    position = position_jitter(width = 0.2),
    aes(col = depth_layer)
  ) +
  geom_boxplot(
    alpha = 0.1,
    outlier.shape = NA,
    width = 0.5,
    aes(col = depth_layer, fill = depth_layer)
  ) +
  scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  scale_fill_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  labs(x = "",
       y = "Standard Effect Size (SES)") +
  theme_light() +
  theme(
    axis.text.x = element_blank(),
    strip.text.x = element_text(size = 14, color = "black"),
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 13)
  )  +
  guides(col = "none", fill = "none")


# by depth layer ----


# # Calculate functional diversity for the observed data ----
obsFD_depth_layer <- mFD::alpha.fd.multidim(
  sp_faxes_coord = sp_faxes_coord_fish[, c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w = depth_fish_biomass,
  scaling = TRUE,
  check_input = TRUE,
  details_returned = F
)

results_index_depth_layer <- obsFD_depth_layer$functional_diversity_indices %>% 
  tibble::rownames_to_column(var = "depth_layer") %>%
  tidyr::pivot_longer(!depth_layer,
                      values_to = "values",
                      names_to = "indice") %>% 
  filter(indice!="sp_richn")

results_index_depth_layer$depth_layer <- factor(
  results_index_depth_layer$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)

ggplot(results_index_depth_layer ,
       aes(x = depth_layer, y = values, fill = depth_layer)) +
  facet_wrap( ~ indice) +
  geom_point(
    alpha = 0.5,
    size = 3,
    position = position_jitter(width = 0.2),
    aes(col = depth_layer))+
  scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  scale_fill_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  labs(x = "",
       y = "Standard Effect Size (SES)") +
  theme_light() +
  theme(
    axis.text.x = element_blank(),
    strip.text.x = element_text(size = 14, color = "black"),
    strip.background = element_rect(fill = "white"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 13)
  )  +
  guides(col = "none", fill = "none")

