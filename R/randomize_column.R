# Define randomization function for traits
randomize_matrix_swap_traits <- function(mat) {
  n_species <- nrow(mat)
  n_traits <- ncol(mat)
  
  randomized_mat <- mat
  for (i in 1:n_traits) {
    randomized_mat[, i] <- sample(mat[, i], replace=TRUE)  # Randomize each column independently
  }
  
  return(randomized_mat)
}

# Number of simulations
n_simulations <- 999

# Keep only "fric" for functional diversity index
indice_name <- c("fric") 

# Calculate functional diversity for the observed data ----
obsFD <- mFD::alpha.fd.multidim(
  sp_faxes_coord = sp_faxes_coord_fish[, c("PC1", "PC2", "PC3", "PC4")],
  asb_sp_w = depth_fish_biomass,
  ind_vect = indice_name,  
  scaling = TRUE,
  check_input = TRUE,
  details_returned = F
)

# Initialize a list to store results for "fric"
resultsRandomFD <- list(
  fric = matrix(
    NA,
    nrow = nrow(depth_fish_biomass),  # Number of assemblages
    ncol = n_simulations,             # Number of simulations
    dimnames = list(rownames(depth_fish_biomass), paste0("Sim.", 1:n_simulations))
  )
)

# Perform simulations
for (sim in 1:n_simulations) {
  
  # Step 1: Randomize the traits
  randomized_traits <- randomize_matrix_swap_traits(fish_traits)
  
  # Step 2: Compute functional distance between species
  sp_dist_fish_random <- mFD::funct.dist(
    sp_tr         = randomized_traits,
    tr_cat        = fish_traits_cat,
    metric        = "gower",
    scale_euclid  = "scale_center",
    ordinal_var   = "classic",
    weight_type   = "equal",
    stop_if_NA    = TRUE
  )
  
  # Step 3: Compute the functional space
  fspaces_quality_fish_random <- mFD::quality.fspaces(
    sp_dist             = sp_dist_fish_random,
    maxdim_pcoa         = 10,
    deviation_weighting = "absolute",
    fdist_scaling       = FALSE,
    fdendro             = "average"
  )
  
  sp_faxes_coord_fish_random <- fspaces_quality_fish_random$"details_fspaces"$"sp_pc_coord"
  
  # Step 4: Compute functional diversity index "fric"
  simFD_cal <- mFD::alpha.fd.multidim(
    sp_faxes_coord = sp_faxes_coord_fish_random[, c("PC1", "PC2", "PC3", "PC4")],
    asb_sp_w = depth_fish_biomass,
    ind_vect = indice_name, 
    scaling = TRUE,
    check_input = TRUE,
    details_returned = FALSE
  )
  
  # Step 5: Store results for "fric"
  resultsRandomFD$fric[, sim] <- simFD_cal$functional_diversity_indices[, "fric"]
}

# Initialize dataframes for mean, standard deviation, effect size, and SES
meanNullFD <- data.frame(fric = rowMeans(resultsRandomFD$fric, na.rm = TRUE))
sdNullFD <- data.frame(fric = apply(resultsRandomFD$fric, 1, sd, na.rm = TRUE))

# Effect size (ES) and standardized effect size (SES)
ES_FD <- data.frame(fric = obsFD$functional_diversity_indices[, "fric"] - meanNullFD$fric)
SES_FD <- data.frame(fric = ES_FD$fric / sdNullFD$fric)

# Combine results into a single dataframe
results_df <- cbind(
  obsFD$functional_diversity_indices,
  meanNullFD = setNames(meanNullFD, paste0("meanNullFD.", names(meanNullFD))),
  sdNullFD = setNames(sdNullFD, paste0("sdNullFD.", names(sdNullFD))),
  ES_FD = setNames(ES_FD, paste0("ES_FD.", names(ES_FD))),
  SES_FD = setNames(SES_FD, paste0("SES_FD.", names(SES_FD)))
)

# Prepare data for plotting (filter only "SES_FD.fric")
results_df_plot <- results_df %>%
  tibble::rownames_to_column(var = "depth_layer") %>%
  tidyr::pivot_longer(!depth_layer,
                      values_to = "values",
                      names_to = "indice") %>%
  filter(indice == "SES_FD.fric")

results_df_plot$depth_layer <- factor(
  results_df_plot$depth_layer,
  levels = c(
    "Epipelagic",
    "Upper mesopelagic",
    "Lower mesopelagic",
    "Bathypelagic"
  )
)

# Plot SES results
ggplot(results_df_plot, aes(x = depth_layer, y = values)) +
  geom_point(size = 3, aes(color = depth_layer)) +
  geom_hline(yintercept = 1.96, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  geom_hline(yintercept = -1.96, linetype = "dashed", color = "gray50", linewidth = 0.8) +
  scale_color_manual(values = c("#FEA520", "#D62246", "#6255B4", "#3C685A")) +
  ylim(-4, 4) +
  labs(
    x = "",
    y = "Standard Effect Size (SES)"
  ) +
  guides(col = "none") +
  theme_light() +
  theme(axis.title = element_text(size = 13),
        axis.text.y = element_text(size = 9))
