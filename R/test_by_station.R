# # by depth

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

 obsFD_div <- obsFD$functional_diversity_indices
# # Null model ----

# # Define the number of replications
nb_rep <- 10

# # Initialize a list to store results of random functional diversity calculations for each index
indices_names <- colnames(obsFD_div)
resultsRandomFD <- list()
for (index_name in indices_names) {
  resultsRandomFD[[index_name]] <-
    matrix(
      NA,
      nrow = nrow(station_sp),
      ncol = nb_rep,
      dimnames = list(rownames(station_sp), paste0("Sim.", 1:nb_rep))
    )
}

# Perform randomization and calculate functional diversity for each replication
for (rep in 1:nb_rep) {
  randomize_mx <-
    picante::randomizeMatrix(samp = station_sp,
                             null.model = "richness",
                             iterations = 1)
  simFD_cal <- mFD::alpha.fd.multidim(
    sp_faxes_coord = sp_faxes_coord_fish[, c("PC1", "PC2", "PC3", "PC4")],
    asb_sp_w = randomize_mx,
    scaling = TRUE,
    check_input = TRUE,
    details_returned = F
  )
  
  simFD_div <- simFD_cal$functional_diversity_indices
  for (index_name in indices_names) {
    simFD_index <- simFD_div[, index_name]
    # Ensure that simFD_index has the same length as the number of rows in station_sp
    if (length(simFD_index) == nrow(station_sp)) {
      resultsRandomFD[[index_name]][, rep] <- simFD_index
    } else {
      stop(
        paste(
          "The length of",
          index_name,
          "does not match the number of rows in station_sp"
        )
      )
    }
  }
}

# Initialize dataframes to store mean, standard deviation, effect size, and standardized effect size

meanNullFD <-
  data.frame(matrix(
    NA,
    nrow = nrow(station_sp),
    ncol = length(indices_names)
  ))

sdNullFD <-
  data.frame(matrix(
    NA,
    nrow = nrow(station_sp),
    ncol = length(indices_names)
  ))

ES_FD <-
  data.frame(matrix(
    NA,
    nrow = nrow(station_sp),
    ncol = length(indices_names)
  ))

SES_FD <-
  data.frame(matrix(
    NA,
    nrow = nrow(station_sp),
    ncol = length(indices_names)
  ))

# Set column names for the dataframes
colnames(meanNullFD) <- indices_names
colnames(sdNullFD) <- indices_names
colnames(ES_FD) <- indices_names
colnames(SES_FD) <- indices_names

# Calculate statistics for each index
for (index_name in indices_names) {
  # Calculate mean and standard deviation of null model FD values for each index
  meanNullFD[, index_name] <-
    rowMeans(resultsRandomFD[[index_name]], na.rm = TRUE)
  sdNullFD[, index_name] <-
    apply(resultsRandomFD[[index_name]], 1, sd, na.rm = TRUE)
  
  # Calculate effect size and standardized effect size for each index
  ES_FD[, index_name] <-
    obsFD_div[, index_name] - meanNullFD[, index_name]
  SES_FD[, index_name] <-
    ES_FD[, index_name] / sdNullFD[, index_name]
}

# Combine all results into a single dataframe
results_df <- cbind(
  obsFD_div,
  meanNullFD = meanNullFD,
  sdNullFD = sdNullFD,
  ES_FD = ES_FD,
  SES_FD = SES_FD
)

# Add row names
rownames(results_df) <- rownames(station_sp)

# Plot
# Output the results dataframe
results_df_plot <- results_df %>%
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
  mutate(indice = stringr::str_replace(indice, "^SES_", "")) %>%
  filter(indice %in% c("FD.fric",
                       "FD.fdis",
                       "FD.feve",
                       "FD.fdiv"))

results_df_plot$depth_layer <- factor(
  results_df_plot$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)

results_df_plot$indice <- factor(
  results_df_plot$indice,
  levels = c("FD.fric",
             "FD.fdis",
             "FD.fdiv",
             "FD.feve"),
  labels = c(
    "Functional richness",
    "Functional dispersion",
    "Functional divergence",
    "Functional evenness"
  )
)

ggplot(results_df_plot,
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
  ) +
  geom_hline(
    yintercept = 1.96,
    linetype = "dashed",
    color = "gray50",
    linewidth = 0.8
  ) +
  geom_hline(
    yintercept = -1.96,
    linetype = "dashed",
    color = "gray50",
    linewidth = 0.8
  ) +
  guides(col = "none", fill = "none")

# ggsave("SES_indices_1000.png", path = "figures", dpi = 700, height = 7, width = 9)
